module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable qualified as F
import Data.IORef
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import System.Timeout (timeout)
import Test.Tasty hiding (withResource)
import Test.Tasty.HUnit

import Data.Pool
import Data.Pool.Internal (LocalPool (..), Pool (..), Queue (..), Stripe (..))
import Data.Pool.Introspection qualified as I

main :: IO ()
main =
  defaultMain . localOption (mkTimeout 60000000)
    $ testGroup
      "resource-pool"
      [ configValidationTests
      , basicTests
      , concurrencyTests
      , introspectionTests
      , stressTests
      ]

----------------------------------------
-- Configuration validation

configValidationTests :: TestTree
configValidationTests =
  testGroup
    "config validation"
    [ testCase "rejects too small poolCacheTTL" $ do
        expectError . newPool $ poolConfig_ 0.4 1
    , testCase "rejects a NaN poolCacheTTL" $ do
        -- A NaN would send the collector into a busy loop.
        expectError . newPool $ poolConfig_ (0 / 0) 1
    , testCase "rejects non-positive poolMaxResources" $ do
        expectError . newPool $ poolConfig_ 100 0
    , testCase "rejects non-positive number of stripes" $ do
        expectError . newPool . setNumStripes (Just 0) $ poolConfig_ 100 1
    , testCase "rejects poolMaxResources smaller than the number of stripes" $ do
        expectError . newPool . setNumStripes (Just 4) $ poolConfig_ 100 2
    ]
  where
    poolConfig_ = defaultPoolConfig (pure ()) (\_ -> pure ())

    expectError :: IO a -> Assertion
    expectError io =
      try @ErrorCall (void io) >>= \case
        Left _ -> pure ()
        Right _ -> assertFailure "expected newPool to error out"

----------------------------------------
-- Basic tests

basicTests :: TestTree
basicTests =
  testGroup
    "basic"
    [ testCase "an idle resource is reused" $ do
        tp <- mkPool 5
        r1 <- withResource (testPool tp) pure
        r2 <- withResource (testPool tp) pure
        assertEqual "the same resource is taken" r1 r2
        readIORef (createdCount tp) >>= assertEqual "created resources" 1
    , testCase "concurrently taken resources are distinct" $ do
        tp <- mkPool 5
        (r1, lp1) <- takeResource (testPool tp)
        (r2, lp2) <- takeResource (testPool tp)
        assertBool "resources are distinct" $ r1 /= r2
        putResource lp1 r1
        putResource lp2 r2
    , testCase "tryTakeResource returns Nothing iff the pool is exhausted" $ do
        tp <- mkPool 1
        Just (r, lp) <- tryTakeResource (testPool tp)
        mr <- tryTakeResource (testPool tp)
        assertBool "Nothing when exhausted" $ isNothing mr
        putResource lp r
        mr2 <- tryTakeResource (testPool tp)
        assertBool "Just when a resource is available" $ isJust mr2
    , testCase "tryWithResource returns Nothing iff the pool is exhausted" $ do
        tp <- mkPool 1
        (r, lp) <- takeResource (testPool tp)
        mr <- tryWithResource (testPool tp) pure
        assertEqual "Nothing when exhausted" Nothing mr
        putResource lp r
        mr2 <- tryWithResource (testPool tp) pure
        assertEqual "Just when a resource is available" (Just r) mr2
    , testCase "withResource destroys the resource on exception" $ do
        tp <- mkPool 5
        r <- try @Boom $ withResource (testPool tp) $ \_ -> throwIO Boom :: IO ()
        assertEqual "the exception is propagated" (Left Boom) r
        readIORef (freedCount tp) >>= assertEqual "freed resources" 1
        -- The pool is still usable and creates a fresh resource.
        _ <- withResource (testPool tp) pure
        readIORef (createdCount tp) >>= assertEqual "created resources" 2
    , testCase "destroyResource frees the resource and its slot" $ do
        tp <- mkPool 1
        (r1, lp) <- takeResource (testPool tp)
        destroyResource (testPool tp) lp r1
        readIORef (freedCount tp) >>= assertEqual "freed resources" 1
        r2 <- withResource (testPool tp) pure
        assertBool "resource is fresh" $ r2 /= r1
    , testCase "destroyAllResources frees all idle resources" $ do
        tp <- mkPool 5
        rs <- replicateM 3 $ takeResource (testPool tp)
        forM_ rs $ \(r, lp) -> putResource lp r
        destroyAllResources (testPool tp)
        readIORef (freedCount tp) >>= assertEqual "freed resources" 3
        -- The pool is still usable and creates a fresh resource.
        _ <- withResource (testPool tp) pure
        readIORef (createdCount tp) >>= assertEqual "created resources" 4
    , testCase "resources are distributed evenly across stripes" $ do
        tp <- mkPoolWith (setNumStripes $ Just 2) 5
        ss <- stripeStates (testPool tp)
        assertEqual "available resources per stripe" [2, 3]
          $ L.sort (map available ss)
    , testCase "idle resources exceeding the TTL are collected" $ do
        createdC <- newIORef (0 :: Int)
        freedC <- newIORef (0 :: Int)
        pool <-
          newPool
            $ defaultPoolConfig
              (atomicModifyIORef' createdC $ \n -> (n + 1, n + 1))
              (\_ -> atomicModifyIORef' freedC $ \n -> (n + 1, ()))
              0.5
              5
        _ <- withResource pool pure
        -- The collector should free the resource promptly after its TTL
        -- expires.
        waitUntil "the resource is collected" $ (== 1) <$> readIORef freedC
        readIORef createdC >>= assertEqual "created resources" 1
    , testCase "the collector runs again after the cache is refilled" $ do
        freedC <- newIORef (0 :: Int)
        pool <-
          newPool
            $ defaultPoolConfig
              (pure ())
              (\_ -> atomicModifyIORef' freedC $ \n -> (n + 1, ()))
              0.5
              5
        _ <- withResource pool pure
        waitUntil "the first resource is collected" $ (== 1) <$> readIORef freedC
        -- The collector went back to sleep on an empty cache; putting a new
        -- resource into it needs to wake it up again.
        _ <- withResource pool pure
        waitUntil "the second resource is collected" $ (== 2) <$> readIORef freedC
    , testCase "entries not yet stale in a collection round are collected later" $ do
        freedC <- newIORef (0 :: Int)
        pool <-
          newPool
            $ defaultPoolConfig
              (pure ())
              (\_ -> atomicModifyIORef' freedC $ \n -> (n + 1, ()))
              0.5
              5
        (r1, lp1) <- takeResource pool
        (r2, lp2) <- takeResource pool
        putResource lp1 r1
        -- Put the second resource back only after a while, so that when the
        -- collector wakes up to free the first one, the second one is not yet
        -- stale and has to be freed in a later collection round, even though
        -- the pool sees no further activity.
        threadDelay 300000
        putResource lp2 r2
        waitUntil "the first resource is collected" $ (>= 1) <$> readIORef freedC
        waitUntil "the second resource is collected" $ (== 2) <$> readIORef freedC
    ]

----------------------------------------
-- Concurrency tests

concurrencyTests :: TestTree
concurrencyTests =
  testGroup
    "concurrency"
    [ testCase "the number of created resources never exceeds the maximum" $ do
        tp <- mkPool 3
        mapConcurrently_
          (\_ -> replicateM_ 10 . withResource (testPool tp) $ \_ -> threadDelay 1000)
          [1 .. 20 :: Int]
        created <- readIORef (createdCount tp)
        assertBool ("created " ++ show created ++ " resources") $ created <= 3
        ss <- stripeStates (testPool tp)
        assertEqual "all permits are available" 3 $ sum (map available ss)
    , testCase "a blocked thread is woken up by putResource" $ do
        tp <- mkPool 1
        (r, lp) <- takeResource (testPool tp)
        done <- newEmptyMVar
        _ <- forkIO $ withResource (testPool tp) pure >>= putMVar done
        waitUntil "the thread is queued"
          $ any ((> 0) . queueLength) <$> stripeStates (testPool tp)
        putResource lp r
        mr <- timeout 5000000 $ takeMVar done
        assertEqual "the returned resource is received" (Just r) mr
    , testCase "a blocked thread is woken up when resource creation fails" $ do
        entered <- newEmptyMVar
        gate <- newEmptyMVar
        let create = do
              putMVar entered ()
              takeMVar gate
              throwIO Boom :: IO Int
        pool <- newPool $ defaultPoolConfig create (\_ -> pure ()) 100 1
        t1 <- newEmptyMVar
        _ <- forkIO $ try @Boom (withResource pool $ \_ -> pure ()) >>= putMVar t1
        takeMVar entered
        t2 <- newEmptyMVar
        _ <- forkIO $ try @Boom (withResource pool $ \_ -> pure ()) >>= putMVar t2
        waitUntil "the second thread is queued"
          $ any ((> 0) . queueLength) <$> stripeStates pool
        -- Fail creation in the first thread. The second one needs to be woken
        -- up and attempt creation itself.
        putMVar gate ()
        takeMVar t1 >>= assertEqual "the first thread failed" (Left Boom)
        mr <- timeout 5000000 $ do
          takeMVar entered
          putMVar gate ()
          takeMVar t2
        assertEqual "the second thread attempted creation" (Just (Left Boom)) mr
    ]

----------------------------------------
-- Introspection tests

introspectionTests :: TestTree
introspectionTests =
  testGroup
    "introspection"
    [ testCase "resource metadata is filled in correctly" $ do
        pool <-
          newPool . setPoolLabel (T.pack "label")
            $ defaultPoolConfig (pure (0 :: Int)) (\_ -> pure ()) 100 1
        (res, lp) <- I.takeResource pool
        assertEqual "poolLabel" (T.pack "label") (I.poolLabel res)
        assertEqual "stripeNumber" 1 (I.stripeNumber res)
        assertEqual "acquisition" I.Immediate (I.acquisition res)
        assertBool "creationTime is set for a fresh resource"
          $ isJust (I.creationTime res)
        putResource lp $ I.resource res
        (res2, lp2) <- I.takeResource pool
        assertEqual "acquisition" I.Immediate (I.acquisition res2)
        assertEqual "creationTime is not set for a cached resource" Nothing
          $ I.creationTime res2
        putResource lp2 $ I.resource res2
    , testCase "acquisition of a blocked thread is Delayed" $ do
        tp <- mkPool 1
        (res, lp) <- I.takeResource (testPool tp)
        done <- newEmptyMVar
        _ <- forkIO $ I.takeResource (testPool tp) >>= putMVar done
        waitUntil "the thread is queued"
          $ any ((> 0) . queueLength) <$> stripeStates (testPool tp)
        putResource lp $ I.resource res
        mr <- timeout 5000000 $ takeMVar done
        case mr of
          Nothing -> assertFailure "the blocked thread wasn't woken up"
          Just (res2, lp2) -> do
            assertEqual "acquisition" I.Delayed (I.acquisition res2)
            putResource lp2 $ I.resource res2
    ]

----------------------------------------
-- Stress tests

stressTests :: TestTree
stressTests =
  testGroup
    "stress"
    [ testCase "pool invariants hold under load (1 stripe)" $ stressTest 1
    , testCase "pool invariants hold under load (4 stripes)" $ stressTest 4
    ]
  where
    stressTest numStripes = do
      let maxResources = 8
      tp <- mkPoolWith (setNumStripes $ Just numStripes) maxResources
      mapConcurrently_
        ( \i -> replicateM_ 50 $ do
            r <- try @Boom . withResource (testPool tp) $ \_ -> do
              threadDelay 100
              -- Some of the threads always fail, destroying the resource.
              when (i `rem` 5 == 0) $ throwIO Boom
            case r of
              Left Boom -> when (i `rem` 5 /= 0) $ throwIO Boom
              Right () -> pure ()
        )
        [1 .. 30 :: Int]
      -- Once the pool is idle again, all permits are available, all live
      -- resources sit in stripe caches and no thread is queued.
      ss <- stripeStates (testPool tp)
      assertEqual "all permits are available" maxResources
        $ sum (map available ss)
      created <- readIORef (createdCount tp)
      freed <- readIORef (freedCount tp)
      assertEqual "all live resources are cached" (created - freed)
        $ sum (map (length . cache) ss)
      assertEqual "no thread is queued" 0 $ sum (map queueLength ss)

----------------------------------------
-- Helpers

data Boom = Boom
  deriving stock (Eq, Show)

instance Exception Boom

data TestPool = TestPool
  { testPool :: Pool Int
  , createdCount :: IORef Int
  , freedCount :: IORef Int
  }

mkPool :: Int -> IO TestPool
mkPool = mkPoolWith id

mkPoolWith :: (PoolConfig Int -> PoolConfig Int) -> Int -> IO TestPool
mkPoolWith adjust maxResources = do
  createdC <- newIORef 0
  freedC <- newIORef 0
  pool <-
    newPool . adjust
      $ defaultPoolConfig
        (atomicModifyIORef' createdC $ \n -> (n + 1, n + 1))
        (\_ -> atomicModifyIORef' freedC $ \n -> (n + 1, ()))
        100
        maxResources
  pure
    TestPool
      { testPool = pool
      , createdCount = createdC
      , freedCount = freedC
      }

stripeStates :: Pool a -> IO [Stripe a]
stripeStates pool = mapM (readTVarIO . stripeVar) . F.toList $ localPools pool

queueLength :: Stripe a -> Int
queueLength stripe = go (queue stripe) + go (queueR stripe)
  where
    go = \case
      Empty -> 0
      Queue _ q -> 1 + go q

-- | Wait (up to 10 seconds) until a condition is met.
waitUntil :: String -> IO Bool -> Assertion
waitUntil label cond = go (100 :: Int)
  where
    go n = do
      ok <- cond
      unless ok
        $ if n == 0
          then assertFailure $ "timed out waiting until " ++ label
          else threadDelay 100000 >> go (n - 1)
