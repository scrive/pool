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
import Data.Hashable (hash)
import Data.IORef
import Data.List qualified as L
import Data.Primitive.SmallArray
import GHC.Clock (getMonotonicTime)
import GHC.Conc (unsafeIOToSTM)

-- | Striped resource pool based on "Control.Concurrent.QSem".
data Pool a = Pool
  { poolConfig :: !(PoolConfig a)
  , localPools :: !(SmallArray (LocalPool a))
  , reaperRef :: !(IORef ())
  }

-- | A single, local pool.
data LocalPool a = LocalPool
  { stripeId :: !Int
  , stripeVar :: !(TVar (Stripe a))
  , cleanerRef :: !(IORef ())
  }

-- | Stripe of a resource pool. If @available@ is 0, the list of threads waiting
-- for a resource (each with an associated 'TMVar') is @queue ++ reverse queueR@
-- to ensure fairness.
data Stripe a = Stripe
  { available :: !Int
  , cache :: ![Entry a]
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
  --
  -- /Note:/ the elapsed time before destroying a resource may be a little
  -- longer than requested, as the collector thread wakes at 1-second intervals.
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

-- | Create a new striped resource pool.
--
-- /Note:/ although the runtime system will destroy all idle resources when the
-- pool is garbage collected, it's recommended to manually call
-- 'destroyAllResources' when you're done with the pool so that the resources
-- are freed up as soon as possible.
newPool :: PoolConfig a -> IO (Pool a)
newPool pc = do
  when (poolCacheTTL pc < 0.5) $ do
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
    -- When the local pool goes out of scope, free its resources.
    void . mkWeakIORef ref $ cleanStripe (const True) (freeResource pc) stripe
    pure
      LocalPool
        { stripeId = n
        , stripeVar = stripe
        , cleanerRef = ref
        }
  mask_ $ do
    ref <- newIORef ()
    collectorA <- forkIOWithUnmask $ \unmask -> unmask $ collector pools
    void . mkWeakIORef ref $ do
      -- When the pool goes out of scope, stop the collector. Resources existing
      -- in stripes will be taken care by their cleaners.
      killThread collectorA
    pure
      Pool
        { poolConfig = pc
        , localPools = pools
        , reaperRef = ref
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

    -- Collect stale resources from the pool once per second.
    collector pools = forever $ do
      threadDelay 1000000
      now <- getMonotonicTime
      let isStale e = now - lastUsed e > poolCacheTTL pc
      mapM_ (cleanStripe isStale (freeResource pc) . stripeVar) pools

-- | Destroy a resource.
--
-- Note that this will ignore any exceptions in the destroy function.
destroyResource :: Pool a -> LocalPool a -> a -> IO ()
destroyResource pool lp a = mask_ $ do
  atomically $ do
    stripe <- readTVar (stripeVar lp)
    newStripe <- signal stripe Nothing
    writeTVar (stripeVar lp) $! newStripe
  freeResource (poolConfig pool) a

-- | Return a resource to the given 'LocalPool'.
putResource :: LocalPool a -> a -> IO ()
putResource lp a = atomically $ do
  stripe <- readTVar (stripeVar lp)
  newStripe <- signal stripe (Just a)
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

-- | Get a local pool.
getLocalPool :: SmallArray (LocalPool a) -> IO (LocalPool a)
getLocalPool pools = do
  sid <-
    if stripes == 1
      then -- If there is just one stripe, there is no choice.
        pure 0
      else do
        capabilities <- getNumCapabilities
        -- If the number of stripes is smaller than the number of capabilities and
        -- doesn't divide it, selecting a stripe by a capability the current
        -- thread runs on wouldn't give equal load distribution across all stripes
        -- (e.g. if there are 2 stripes and 3 capabilities, stripe 0 would be used
        -- by capability 0 and 2, while stripe 1 would only be used by capability
        -- 1, a 100% load difference). In such case we select based on the id of a
        -- thread.
        if stripes < capabilities && capabilities `rem` stripes /= 0
          then hash <$> myThreadId
          else fmap fst . threadCapability =<< myThreadId
  pure $ pools `indexSmallArray` (sid `rem` stripes)
  where
    stripes = sizeofSmallArray pools

-- | Wait for the resource to be put into a given 'TMVar'.
waitForResource :: TVar (Stripe a) -> TMVar (Maybe a) -> IO (Maybe a)
waitForResource mstripe q = atomically (takeTMVar q) `onException` cleanup
  where
    cleanup = atomically $ do
      stripe <- readTVar mstripe
      newStripe <-
        tryTakeTMVar q >>= \case
          Just ma -> do
            -- Between entering the exception handler and taking ownership of
            -- the stripe we got the resource we wanted. We don't need it
            -- anymore though, so pass it to someone else.
            signal stripe ma
          Nothing -> do
            -- If we're still waiting, fill up the TMVar with an undefined value
            -- so that 'signal' can discard our TMVar from the queue.
            putTMVar q $ error "unreachable"
            pure stripe
      writeTVar mstripe $! newStripe

-- | If an exception is received while a resource is being created, restore the
-- original size of the stripe.
restoreSize :: TVar (Stripe a) -> IO ()
restoreSize mstripe = atomically $ do
  modifyTVar' mstripe $ \stripe -> stripe {available = available stripe + 1}

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
  -- We need to ignore exceptions in the 'free' function, otherwise if an
  -- exception is thrown half-way, we leak the rest of the resources. Also,
  -- asynchronous exceptions need to be hard masked here we need to run 'free'
  -- for all resources.
  uninterruptibleMask $ \release -> do
    rs <- forM stale $ try @SomeException . release . free
    -- If any async exception arrived in between, propagate it.
    rethrowFirstAsyncException $ lefts rs
  where
    rethrowFirstAsyncException = \case
      [] -> pure ()
      e : es
        | Just SomeAsyncException {} <- fromException e -> throwIO e
        | otherwise -> rethrowFirstAsyncException es

signal :: forall a. Stripe a -> Maybe a -> STM (Stripe a)
signal stripe ma =
  if available stripe == 0
    then loop (queue stripe) (queueR stripe)
    else do
      newCache <- case ma of
        Just a -> do
          now <- unsafeIOToSTM getMonotonicTime
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
