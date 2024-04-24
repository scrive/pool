{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , tryWithResourceThenCreate
  , tryWithResourceNThenCreate

    -- * Compatibility with 0.2
  , createPool
  ) where

import Control.Concurrent
import Control.Exception
import qualified Control.Exception as AllException
import qualified Control.Exception.Safe as SyncException
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
  r <- unmask (act res) `onException` destroyResource pool localPool res
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
      putMVar (stripeVar lp) $! stripe {queueR = Queue q (queueR stripe)}
      waitForResource (stripeVar lp) q >>= \case
        Just a -> pure (a, lp)
        Nothing -> do
          a <- createResource (poolConfig pool) `onException` restoreSize (stripeVar lp)
          pure (a, lp)
    else takeAvailableResource pool lp stripe

-- | A variant of 'withResource' that doesn't execute the action and returns
-- 'Nothing' instead of blocking if the local pool is exhausted.
tryWithResource :: Pool a -> (a -> IO r) -> IO (Maybe r)
tryWithResource pool act = mask $ \unmask ->
  tryTakeResource pool >>= \case
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
createPool create free numStripes idleTime maxResources =
  newPool
    PoolConfig
      { createResource = create
      , freeResource = free
      , poolCacheTTL = realToFrac idleTime
      , poolMaxResources = numStripes * maxResources
      , poolNumStripes = Just numStripes
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
    putMVar (stripeVar lp) $! stripe {available = available stripe - 1}
    a <- createResource (poolConfig pool) `onException` restoreSize (stripeVar lp)
    pure (a, lp)
  Entry a _ : as -> do
    putMVar (stripeVar lp) $!
      stripe
        { available = available stripe - 1
        , cache = as
        }
    pure (a, lp)

-- | This does something like 'withResource' initially, in that it tries to get a resource from the pool and run an action on it,
-- returning the resource to the pool if the action doesn't throw an exception, but destroying if the action does throw an exception.
-- 
-- This function does the same as 'withResource' if the exception thrown is a asynchonous exception. 
-- But, in this function, if the exception is a synchronous exceptions, unlike 'withResource', we just don't immediately rethrow.
-- We still destroy the resource, like 'withResource', but instead of rethrowing we run 
-- 'handlePoolResourceException' on the exception (this should just be a logging function). 
-- Then we actually create a fresh new resource, not just pull another existing one from the pool.
-- We then try to run the action on this fresh resource. If this succeeds, we put this fresh resource into the pool.
-- If this fails though, we destroy the fresh resource, and rethrow the exception.
-- 
-- What is this useful for? Sometimes you have a pool of resources, that may just all break at the same time for some reason.
-- For example, someone may have restarted your database, so all of your connections are now invalid.
-- 
-- Lets say you have a pool of 100 database connections. Now you've got a pool of potentially 100 dead connections.
-- 
-- The only way to get a good connection is to run 'withResource' 101 times, which will eventually destroy all the bad connections.
-- 
-- Also, maybe the database isn't back up yet. Now even retrying 101 times isn't going to get you a good connection.
-- 
-- Note that unlike 'withResource' that will reduce the number of resources in the pool if the action fails with an exception,
-- 'tryWithResourceThenCreate' with only reduce the number of resources in the pool if:
-- 
-- The action applied to the resource from the pool fails with an asynchronous exception OR
-- (
--   The action applied to the resource from the pool fails with an synchronous exception AND
--   (
--     The creation of a new resource fails with an exception OR
--     The action applied to the new resource fails with an exception
--   )
-- )
-- 
-- So basically, if your pool resource fails, but your fresh resource succeeds, you've just replaced the broken resource with a fresh one.
-- 
-- It's probably worthwhile to at least a few times to try to find good existing resource in the pool before creating a new one.
-- 
-- So I would recommend is running 'withResource' before this, maybe a few times, and then running 'tryWithResourceThenCreate'.
-- 
-- And indeed, that's what 'tryWithResourceNThenCreate' does.
tryWithResourceThenCreate :: forall r a. (SomeException -> IO ()) -> Pool a -> (a -> IO r) -> IO r
tryWithResourceThenCreate handlePoolResourceException pool@Pool{poolConfig = PoolConfig{createResource, freeResource} } act = 
  AllException.mask $ \unmask -> do
    (res, localPool) <- takeResource pool
    -- This will catch any synchonous exceptions in `tryR`. But any async exceptions will be flow to the 'onException` handler
    -- to destroy the resource and just rethrow the exception.
    tryR :: (Either SomeException r) <- 
      (SyncException.try $ unmask (act res)) `AllException.onException` destroyResource pool localPool res
    case tryR of
      Right r -> do
        -- We've ran the action sucessfully on the resource in the pool. Just put it back in the pool and return.
        putResource localPool res
        pure r
      Left (e :: SomeException) -> do
        -- We've failed to run the action on the resource in the pool.
        -- So destroy it...
        destroyResource pool localPool res
        -- Run our handler for the first exception
        unmask $ handlePoolResourceException e
        -- And create an entirely new resource
        newRes <- unmask createResource
        -- Run the action on the new resource. If this fails for any reason, free the resource and rethrow the exception
        result <- AllException.onException (unmask (act newRes)) (freeResource newRes)
        -- If we've got here, we've successfully run the action on the new resource. Put this fresh resource in the pool
        putResource localPool newRes
        pure result

-- | Run 'withResource' 'n' times. Each time we run it if it throws we run 'handlePoolResourceException' and try again.
-- If 'withResource' has failed 'n' times, then we run 'tryWithResourceThenCreate', 
-- which is basically like running 'withResource' one more time, but instead if it fails it
-- creates a fresh resource instead of getting one from the pool.
-- 
-- What this means that 'tryWithResourceNThenCreate' will potentially run 'act' (n + 2) times.
tryWithResourceNThenCreate :: forall r a. (SomeException -> IO ()) -> Int -> Pool a -> (a -> IO r) -> IO r
tryWithResourceNThenCreate handlePoolResourceException n pool act = go n where
  go :: Int -> IO r
  go i = case (i <= 0) of
    True -> tryWithResourceThenCreate handlePoolResourceException pool act
    False -> 
      withResource pool act `SyncException.catch` \(e :: SomeException) -> do
        handlePoolResourceException e
        go (i - 1)
