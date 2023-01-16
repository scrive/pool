-- | A high-performance pooling abstraction for managing flexibly-sized
-- collections of resources such as database connections.
module Data.Pool
  ( -- * Pool
    Pool
  , LocalPool
  , newPool

    -- ** Configuration
  , PoolConfig
  , defaultPoolConfig
  , setNumStripes

    -- * Resource management
  , withResource
  , takeResource
  , tryWithResource
  , tryTakeResource
  , putResource
  , destroyResource
  , destroyAllResources

    -- * Compatibility with 0.2
  , createPool
  ) where

import Control.Concurrent
import Control.Exception
import Data.Time (NominalDiffTime)

import Data.Pool.Internal

-- | Take a resource from the pool, perform an action with it and return it to
-- the pool afterwards.
--
-- * If the pool has an idle resource available, it is used immediately.
--
-- * Otherwise, if the maximum number of resources has not yet been reached, a
--   new resource is created and used.
--
-- * If the maximum number of resources has been reached, this function blocks
--   until a resource becomes available.
--
-- If the action throws an exception of any type, the resource is destroyed and
-- not returned to the pool.
--
-- It probably goes without saying that you should never manually destroy a
-- pooled resource, as doing so will almost certainly cause a subsequent user
-- (who expects the resource to be valid) to throw an exception.
withResource :: Pool a -> (a -> IO r) -> IO r
withResource pool act = mask $ \unmask -> do
  (res, localPool) <- takeResource pool
  r                <- unmask (act res) `onException` destroyResource pool localPool res
  putResource localPool res
  pure r

-- | Take a resource from the pool, following the same results as
-- 'withResource'.
--
-- /Note:/ this function returns both a resource and the 'LocalPool' it came
-- from so that it may either be destroyed (via 'destroyResource') or returned
-- to the pool (via 'putResource').
takeResource :: Pool a -> IO (a, LocalPool a)
takeResource pool = mask_ $ do
  lp <- getLocalPool (localPools pool)
  stripe <- takeMVar (stripeVar lp)
  if available stripe == 0
    then do
      q <- newEmptyMVar
      putMVar (stripeVar lp) $! stripe { queueR = Queue q (queueR stripe) }
      waitForResource (stripeVar lp) q >>= \case
        Just a  -> pure (a, lp)
        Nothing -> do
          a <- createResource (poolConfig pool) `onException` restoreSize (stripeVar lp)
          pure (a, lp)
    else takeAvailableResource pool lp stripe

-- | A variant of 'withResource' that doesn't execute the action and returns
-- 'Nothing' instead of blocking if the local pool is exhausted.
tryWithResource :: Pool a -> (a -> IO r) -> IO (Maybe r)
tryWithResource pool act = mask $ \unmask -> tryTakeResource pool >>= \case
  Just (res, localPool) -> do
    r <- unmask (act res) `onException` destroyResource pool localPool res
    putResource localPool res
    pure (Just r)
  Nothing -> pure Nothing

-- | A variant of 'takeResource' that returns 'Nothing' instead of blocking if
-- the local pool is exhausted.
tryTakeResource :: Pool a -> IO (Maybe (a, LocalPool a))
tryTakeResource pool = mask_ $ do
  lp <- getLocalPool (localPools pool)
  stripe <- takeMVar (stripeVar lp)
  if available stripe == 0
    then do
      putMVar (stripeVar lp) stripe
      pure Nothing
    else Just <$> takeAvailableResource pool lp stripe

{-# DEPRECATED createPool "Use newPool instead" #-}
-- | Provided for compatibility with @resource-pool < 0.3@.
--
-- Use 'newPool' instead.
createPool :: IO a -> (a -> IO ()) -> Int -> NominalDiffTime -> Int -> IO (Pool a)
createPool create free numStripes idleTime maxResources = newPool PoolConfig
  { createResource   = create
  , freeResource     = free
  , poolCacheTTL     = realToFrac idleTime
  , poolMaxResources = numStripes * maxResources
  , poolNumStripes   = Just numStripes
  }

----------------------------------------
-- Helpers

takeAvailableResource
  :: Pool a
  -> LocalPool a
  -> Stripe a
  -> IO (a, LocalPool a)
takeAvailableResource pool lp stripe = case cache stripe of
  [] -> do
    putMVar (stripeVar lp) $! stripe { available = available stripe - 1 }
    a <- createResource (poolConfig pool) `onException` restoreSize (stripeVar lp)
    pure (a, lp)
  Entry a _ : as -> do
    putMVar (stripeVar lp) $! stripe
     { available = available stripe - 1
     , cache = as
     }
    pure (a, lp)
