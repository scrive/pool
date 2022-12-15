{-# language OverloadedStrings #-}

module Data.PoolSpec where

import GHC.Clock
import Control.Concurrent.Async
import Control.Monad
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Pool
import Data.Pool.Internal
import System.Timeout
import Control.Concurrent

spec :: Spec
spec = do
  let
    mkPool action =
      newPool PoolConfig
        { createResource = action
        , freeResource = \() -> pure ()
        , poolCacheTTL = 0.5
        , poolMaxResources = 1
        , poolNumStripes = Just 1
        }
    waitTime =
      10000
    waitAction =
      threadDelay waitTime

  describe "createPool" $ do
    it "does not error for any legal set of inputs" $ hedgehog $ do
      numStripes <- forAll $ Gen.integral $ Range.linear 1 100
      idleTime <- forAll $ Gen.realFrac_ $ Range.linearFrac 0.5 100
      maxResources <- forAll $ Gen.integral $ Range.linear 1 100
      void $  evalIO $ createPool (pure ()) (\_ -> pure ()) numStripes idleTime maxResources

  describe "takeResource" $ do
    before (mkPool (pure ())) $ do
      it "works when there is a resource available" $ \pool -> do
        (a, _) <- takeResource pool
        a `shouldBe` ()

      it "blocks when there is not a resource available" $ \pool -> do
        poolAvailableResources pool
          `shouldReturn`
            1
        (a, _) <- takeResource pool
        a `shouldBe` ()
        poolAvailableResources pool
          `shouldReturn`
            0

        mresult <- timeout 100 $ takeResource pool
        void mresult `shouldBe` Nothing

      it "returns when a resource is made available" $ \pool -> do
        poolAvailableResources pool
          `shouldReturn`
            1
        (a, lp) <- takeResource pool
        a `shouldBe` ()
        poolAvailableResources pool
          `shouldReturn`
            0

        mresult <- timeout 100 $ takeResource pool
        void mresult `shouldBe` Nothing

        (mresult', ()) <- concurrently
          (do
            timeout waitTime $ takeResource pool)
          (do
            threadDelay (waitTime `div` 2)
            putResource lp a)
        void mresult' `shouldBe` Just ()

    before (mkPool waitAction) $ do
      it "returns a resource when it is timed out" $ \pool -> do
        start <- getMonotonicTime
        let timeoutTime = waitTime `div` 2
        mresult <- timeout timeoutTime $ takeResource pool
        end <- getMonotonicTime
        let
          -- end and start are both in seconds, so to get to microseconds, we
          -- need to make them much bigger. at the same time, the RTS is
          -- going to have some amount of drift, so we don't want ot be too
          -- precise.
          timeDiff =
            100 * (end - start)
          eps =
            timeDiff - fromIntegral (timeoutTime `div` 10000)

        abs eps `shouldSatisfy` (< 1)

        void mresult `shouldBe` Nothing
        poolAvailableResources pool
          `shouldReturn`
            1
        ((), _) <- takeResource pool
        poolAvailableResources pool
          `shouldReturn`
            0

  describe "withResourceTimeout" $ do
    before (mkPool waitAction) $ do
      describe "when resource available" $ do
        it "provides a resource if fast enough" $ \pool -> do
          poolAvailableResources pool `shouldReturn` 1

          withResourceTimeout (waitTime * 2) pool $ \munit -> do
            munit `shouldBe` Just ()

        it "provides Nothing when timeout happens" $ \pool -> do

          withResourceTimeout (waitTime `div` 2) pool $ \munit -> do
            munit `shouldBe` Nothing

      describe "when resource not available" $ do
        it "waits until one is available" $ \pool -> do
          (a, lp) <- takeResource pool

          var <- newEmptyMVar
          _ <- forkIO $ do
            takeMVar var
            putResource lp a

          -- definitely won't happen since the resource hasn't been returned
          -- yet
          withResourceTimeout (waitTime * 2) pool $ \munit -> do
            munit `shouldBe` Nothing

          _ <- forkIO $ do
            -- fork the thread which will fill the signal to return the
            -- resource
            threadDelay $ waitTime * 2
            putMVar var ()

          withResourceTimeout (waitTime * 3) pool $ \munit -> do
            -- since we're waiting longer than the forked thread will take,
            -- this should succeed
            munit `shouldBe` Just ()
