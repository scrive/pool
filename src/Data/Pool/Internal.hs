{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details for "Data.Pool".
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Data.Pool.Internal where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Either
import Data.Function
import Data.Hashable (hash)
import Data.IORef
import Data.List qualified as L
import Data.Primitive.SmallArray
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Conc (labelThread, unsafeIOToSTM)

-- | Striped resource pool based on "Control.Concurrent.QSem".
data Pool a = Pool
  { poolConfig :: !(PoolConfig a)
  , localPools :: !(SmallArray (LocalPool a))
  }

-- | A single, local pool.
data LocalPool a = LocalPool
  { stripeId :: !Int
  , stripeVar :: !(TVar (Stripe a))
  , wakeupSem :: !WakeupSem
  , cleanerRef :: !(IORef ())
  }

-- | Stripe of a resource pool. If @available@ is 0, the list of threads waiting
-- for a resource (each with an associated 'TMVar') is @queue ++ reverse queueR@
-- to ensure fairness.
data Stripe a = Stripe
  { available :: !Int
  , cache :: ![Entry a]
  -- ^ Ordered by 'lastUsed', newest first (required by collector threads).
  , queue :: !(Queue a)
  , queueR :: !(Queue a)
  }

-- | An existing resource currently sitting in a pool.
data Entry a = Entry
  { entry :: a
  , lastUsed :: !Double
  }

-- | A queue of TMVarS corresponding to threads waiting for resources.
--
-- Basically a monomorphic list to save two pointer indirections.
data Queue a = Queue !(TMVar (Maybe a)) (Queue a) | Empty

-- | Configuration of a 'Pool'.
data PoolConfig a = PoolConfig
  { createResource :: !(IO a)
  , freeResource :: !(a -> IO ())
  , poolCacheTTL :: !Double
  , poolMaxResources :: !Int
  , poolNumStripes :: !(Maybe Int)
  , pcLabel :: !T.Text
  }

-- | Create a 'PoolConfig' with optional parameters having default values.
--
-- For setting optional parameters have a look at:
--
-- - 'setNumStripes'
--
-- @since 0.4.0.0
defaultPoolConfig
  :: IO a
  -- ^ The action that creates a new resource.
  -> (a -> IO ())
  -- ^ The action that destroys an existing resource.
  -> Double
  -- ^ The number of seconds for which an unused resource is kept around. The
  -- smallest acceptable value is @0.5@.
  -> Int
  -- ^ The maximum number of resources to keep open __across all stripes__. The
  -- smallest acceptable value is @1@ per stripe.
  --
  -- /Note:/ if the number of stripes does not divide the number of resources,
  -- some of the stripes will have 1 more resource available than the others.
  -> PoolConfig a
defaultPoolConfig create free cacheTTL maxResources =
  PoolConfig
    { createResource = create
    , freeResource = free
    , poolCacheTTL = cacheTTL
    , poolMaxResources = maxResources
    , poolNumStripes = Just 1
    , pcLabel = T.empty
    }

-- | Set the number of stripes (sub-pools) in the pool.
--
-- If not explicitly set, the default number of stripes is 1, which should be
-- good for typical use (when in doubt, profile your application first).
--
-- If set to 'Nothing', the pool will create the number of stripes equal to the
-- number of capabilities.
--
-- /Note:/ usage of multiple stripes reduces contention, but can also result in
-- suboptimal use of resources since stripes are separated from each other.
--
-- @since 0.4.0.0
setNumStripes :: Maybe Int -> PoolConfig a -> PoolConfig a
setNumStripes numStripes pc = pc {poolNumStripes = numStripes}

-- | Assign a label to the pool.
--
-- The label will appear in a label of the collector thread as well as
-- t'Data.Pool.Introspection.Resource'.
--
-- @since 0.5.0.0
setPoolLabel :: T.Text -> PoolConfig a -> PoolConfig a
setPoolLabel label pc = pc {pcLabel = label}

-- | Create a new striped resource pool.
--
-- /Note:/ although the runtime system will destroy all idle resources when the
-- pool is garbage collected, it's recommended to manually call
-- 'destroyAllResources' when you're done with the pool so that the resources
-- are freed up as soon as possible.
newPool :: forall a. PoolConfig a -> IO (Pool a)
newPool pc = do
  -- Arranged so that NaN is also rejected as it breaks the collector thread.
  unless (poolCacheTTL pc >= 0.5) $ do
    error "poolCacheTTL must be at least 0.5"
  when (poolMaxResources pc < 1) $ do
    error "poolMaxResources must be at least 1"
  numStripes <- maybe getNumCapabilities pure (poolNumStripes pc)
  when (numStripes < 1) $ do
    error "numStripes must be at least 1"
  when (poolMaxResources pc < numStripes) $ do
    error "poolMaxResources must not be smaller than numStripes"
  let mkArray = fmap (smallArrayFromListN numStripes)
  pools <- mkArray . forM (stripeResources numStripes) $ \(n, resources) -> do
    ref <- newIORef ()
    stripe <-
      newTVarIO
        Stripe
          { available = resources
          , cache = []
          , queue = Empty
          , queueR = Empty
          }
    sem <- newWakeupSem
    mask_ $ do
      -- The collector must not reference 'ref', otherwise the finalizer below
      -- would never run.
      collectorId <- forkIOWithUnmask $ \unmask -> unmask $ do
        tid <- myThreadId
        labelThread tid
          $ "resource-pool: collector #"
            ++ show n
            ++ " ("
            ++ T.unpack (pcLabel pc)
            ++ ")"
        collector sem stripe
      void . mkWeakIORef ref $ do
        -- When the local pool goes out of scope, stop its collector and free
        -- its resources.
        killThread collectorId
        cleanStripe (const True) (freeResource pc) stripe
    pure
      LocalPool
        { stripeId = n
        , stripeVar = stripe
        , wakeupSem = sem
        , cleanerRef = ref
        }
  pure
    Pool
      { poolConfig = pc
      , localPools = pools
      }
  where
    stripeResources :: Int -> [(Int, Int)]
    stripeResources numStripes =
      let (base, rest) = quotRem (poolMaxResources pc) numStripes
      in zip [1 .. numStripes] $ addRest (replicate numStripes base) rest
      where
        addRest [] = error "unreachable"
        addRest acc@(r : rs) = \case
          0 -> acc
          rest -> r + 1 : addRest rs (rest - 1)

    collector :: WakeupSem -> TVar (Stripe a) -> IO r
    collector sem stripe = forever $ do
      atomically $ wakeupWait sem
      -- The wakeup signal means that a resource was just put into the empty
      -- cache, so neither it nor any resource cached after it can expire
      -- earlier than TTL from now.
      --
      -- Waiting a full TTL before looking at the cache also caps signal-driven
      -- wakeups at one per TTL when resources are rapidly taken from and put
      -- back into an almost-empty cache.
      waitUntil . (+ poolCacheTTL pc) =<< getMonotonicTime
      fix $ \loop ->
        (cache <$> readTVarIO stripe) >>= \case
          [] -> pure ()
          entries -> do
            -- Nothing can expire before the last entry.
            waitUntil $ lastUsed (L.last entries) + poolCacheTTL pc
            now <- getMonotonicTime
            let isStale e = now - lastUsed e > poolCacheTTL pc
            cleanStripe isStale (freeResource pc) stripe
            loop

    waitUntil :: Double -> IO ()
    waitUntil deadline = do
      now <- getMonotonicTime
      let micros = (deadline - now) * 1000000
      when (micros > 0) $ do
        threadDelay
          $ if micros >= fromIntegral (maxBound :: Int)
            then maxBound
            else ceiling micros

-- | Destroy a resource.
--
-- /Note:/ since version 0.5.0.0 exceptions thrown by the destroy function are
-- no longer ignored.
destroyResource :: Pool a -> LocalPool a -> a -> IO ()
destroyResource pool lp a = mask_ $ do
  atomically $ do
    stripe <- readTVar (stripeVar lp)
    newStripe <- signal lp stripe Nothing
    writeTVar (stripeVar lp) $! newStripe
  freeResource (poolConfig pool) a

-- | Return a resource to the given 'LocalPool'.
putResource :: LocalPool a -> a -> IO ()
putResource lp a = atomically $ do
  stripe <- readTVar (stripeVar lp)
  newStripe <- signal lp stripe (Just a)
  writeTVar (stripeVar lp) $! newStripe

-- | Destroy all resources in all stripes in the pool.
--
-- Note that this will ignore any exceptions in the destroy function.
--
-- This function is useful when you detect that all resources in the pool are
-- broken. For example after a database has been restarted all connections
-- opened before the restart will be broken. In that case it's better to close
-- those connections so that 'takeResource' won't take a broken connection from
-- the pool but will open a new connection instead.
--
-- Another use-case for this function is that when you know you are done with
-- the pool you can destroy all idle resources immediately instead of waiting on
-- the garbage collector to destroy them, thus freeing up those resources
-- sooner.
destroyAllResources :: Pool a -> IO ()
destroyAllResources pool = forM_ (localPools pool) $ \lp -> do
  cleanStripe (const True) (freeResource (poolConfig pool)) (stripeVar lp)

----------------------------------------
-- Helpers

-- | Binary semaphore for signaling a collector thread to wake up.
newtype WakeupSem = WakeupSem (TVar Bool)

newWakeupSem :: IO WakeupSem
newWakeupSem = WakeupSem <$> newTVarIO False

wakeupSignal :: WakeupSem -> STM ()
wakeupSignal (WakeupSem var) = writeTVar var True

wakeupWait :: WakeupSem -> STM ()
wakeupWait (WakeupSem var) = do
  signaled <- readTVar var
  if signaled
    then writeTVar var False
    else retry

----------------------------------------

-- | Get a local pool.
getLocalPool :: SmallArray (LocalPool a) -> IO (LocalPool a)
getLocalPool pools = do
  sid <-
    if stripes == 1
      then -- If there is just one stripe, there is no choice.
        pure 0
      else do
        capabilities <- getNumCapabilities
        -- Selecting a stripe by a capability the current thread runs on gives
        -- equal load distribution only if the number of stripes divides the
        -- number of capabilities. Otherwise:
        --
        -- - If the number of stripes is smaller than the number of
        --   capabilities, load distribution across all stripes wouldn't be
        --   equal (e.g. if there are 2 stripes and 3 capabilities, stripe 0
        --   would be used by capability 0 and 2, while stripe 1 would only be
        --   used by capability 1, a 100% load difference).
        --
        -- - If the number of stripes is larger than the number of capabilities,
        --   stripes with an id larger than the highest capability would never
        --   be selected.
        --
        -- In such cases we select based on the id of a thread.
        if capabilities `rem` stripes /= 0
          then hash <$> myThreadId
          else fmap fst . threadCapability =<< myThreadId
  -- 'mod' is used instead of 'rem' since the hash of a thread id might be
  -- negative, which would result in an out-of-bounds array access.
  pure $ pools `indexSmallArray` (sid `mod` stripes)
  where
    stripes = sizeofSmallArray pools

-- | Wait for the resource to be put into a given 'TMVar'.
waitForResource :: LocalPool a -> TMVar (Maybe a) -> IO (Maybe a)
waitForResource lp q = atomically (takeTMVar q) `onException` cleanup
  where
    cleanup = atomically $ do
      stripe <- readTVar (stripeVar lp)
      newStripe <-
        tryTakeTMVar q >>= \case
          Just ma -> do
            -- Between entering the exception handler and taking ownership of
            -- the stripe we got the resource we wanted. We don't need it
            -- anymore though, so pass it to someone else.
            signal lp stripe ma
          Nothing -> do
            -- If we're still waiting, fill up the TMVar with an undefined value
            -- so that 'signal' can discard our TMVar from the queue.
            putTMVar q $ error "unreachable"
            pure stripe
      writeTVar (stripeVar lp) $! newStripe

-- | If an exception is received while a resource is being created, restore the
-- original size of the stripe.
restoreSize :: LocalPool a -> IO ()
restoreSize lp = atomically $ do
  stripe <- readTVar (stripeVar lp)
  -- Signal needs to be called so that if there are threads waiting for a
  -- resource, one of them wakes up and attempts the creation itself.
  newStripe <- signal lp stripe Nothing
  writeTVar (stripeVar lp) $! newStripe

-- | Free resource entries in the stripes that fulfil a given condition.
cleanStripe
  :: (Entry a -> Bool)
  -> (a -> IO ())
  -> TVar (Stripe a)
  -> IO ()
cleanStripe isStale free mstripe = mask_ $ do
  -- Asynchronous exceptions need to be masked here to prevent leaking of
  -- 'stale' resources before they're freed.
  stale <- atomically $ do
    stripe <- readTVar mstripe
    let (stale, fresh) = L.partition isStale (cache stripe)
    -- There's no need to update 'available' here because it only tracks
    -- the number of resources taken from the pool.
    writeTVar mstripe $! stripe {cache = fresh}
    pure $ map entry stale
  -- We need to catch all exceptions in the 'free' function, otherwise if an
  -- exception was thrown half-way through the traversal of 'stale', we would
  -- leak the rest of the resources.
  --
  -- The loop outside of a call to 'free' is not interruptible, so asynchronous
  -- exceptions can only be delivered inside 'free'. If such a situation arises,
  -- propagate the first one we got.
  rs <- forM stale $ try @SomeException . free
  rethrowFirstAsyncException $ lefts rs
  where
    rethrowFirstAsyncException = \case
      [] -> pure ()
      e : es
        | Just SomeAsyncException {} <- fromException e -> throwIO e
        | otherwise -> rethrowFirstAsyncException es

signal :: forall a. LocalPool a -> Stripe a -> Maybe a -> STM (Stripe a)
signal lp stripe ma =
  -- When cache changes from empty to non-empty, the collector needs to be
  -- signaled via wakeupSem.
  if available stripe == 0
    then loop (queue stripe) (queueR stripe)
    else do
      newCache <- case ma of
        Just a -> do
          now <- unsafeIOToSTM getMonotonicTime
          when (null $ cache stripe) $ do
            wakeupSignal $ wakeupSem lp
          pure $ Entry a now : cache stripe
        Nothing -> pure $ cache stripe
      pure
        stripe
          { available = available stripe + 1
          , cache = newCache
          }
  where
    loop :: Queue a -> Queue a -> STM (Stripe a)
    loop Empty Empty = do
      newCache <- case ma of
        Just a -> do
          now <- unsafeIOToSTM getMonotonicTime
          wakeupSignal $ wakeupSem lp
          pure [Entry a now]
        Nothing -> pure []
      pure
        Stripe
          { available = 1
          , cache = newCache
          , queue = Empty
          , queueR = Empty
          }
    loop Empty qR = loop (reverseQueue qR) Empty
    loop (Queue q qs) qR =
      tryPutTMVar q ma >>= \case
        -- This fails when 'waitForResource' went into the exception handler and
        -- filled the TMVar (with an undefined value) itself. In such case we
        -- simply ignore it.
        False -> loop qs qR
        True ->
          pure
            stripe
              { available = 0
              , queue = qs
              , queueR = qR
              }

    reverseQueue :: Queue a -> Queue a
    reverseQueue = go Empty
      where
        go acc = \case
          Empty -> acc
          Queue x xs -> go (Queue x acc) xs
