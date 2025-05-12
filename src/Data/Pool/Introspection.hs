-- | A variant of "Data.Pool" with introspection capabilities.
module Data.Pool.Introspection
  ( -- * Pool
    Pool
  , LocalPool
  , newPool

    -- ** Configuration
  , PoolConfig
  , defaultPoolConfig
  , setNumStripes

    -- * Resource management
  , Resource (..)
  , Acquisition (..)
  , withResource
  , takeResource
  , tryWithResource
  , tryTakeResource
  , putResource
  , destroyResource
  , destroyAllResources
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)

import Data.Pool.Internal

-- | A resource taken from the pool along with additional information.
data Resource a = Resource
  { resource :: a
  , stripeNumber :: !Int
  , availableResources :: !Int
  , acquisition :: !Acquisition
  , acquisitionTime :: !Double
  , creationTime :: !(Maybe Double)
  }
  deriving (Eq, Show, Generic)

-- | Describes how a resource was acquired from the pool.
data Acquisition
  = -- | A resource was taken from the pool immediately.
    Immediate
  | -- | The thread had to wait until a resource was released.
    Delayed
  deriving (Eq, Show, Generic)

-- | 'Data.Pool.withResource' with introspection capabilities.
withResource :: Pool a -> (Resource a -> IO r) -> IO r
withResource pool act = mask $ \unmask -> do
  (res, localPool) <- takeResource pool
  r <- unmask (act res) `onException` destroyResource pool localPool (resource res)
  putResource localPool (resource res)
  pure r

-- | 'Data.Pool.takeResource' with introspection capabilities.
takeResource :: Pool a -> IO (Resource a, LocalPool a)
takeResource pool = mask_ $ do
  t1 <- getMonotonicTime
  lp <- getLocalPool (localPools pool)
  join . atomically $ do
    stripe <- readTVar (stripeVar lp)
    if available stripe == 0
      then do
        q <- newEmptyTMVar
        writeTVar (stripeVar lp) $! stripe {queueR = Queue q (queueR stripe)}
        pure $
          waitForResource (stripeVar lp) q >>= \case
            Just a -> do
              t2 <- getMonotonicTime
              let res =
                    Resource
                      { resource = a
                      , stripeNumber = stripeId lp
                      , availableResources = 0
                      , acquisition = Delayed
                      , acquisitionTime = t2 - t1
                      , creationTime = Nothing
                      }
              pure (res, lp)
            Nothing -> do
              t2 <- getMonotonicTime
              a <- createResource (poolConfig pool) `onException` restoreSize (stripeVar lp)
              t3 <- getMonotonicTime
              let res =
                    Resource
                      { resource = a
                      , stripeNumber = stripeId lp
                      , availableResources = 0
                      , acquisition = Delayed
                      , acquisitionTime = t2 - t1
                      , creationTime = Just $! t3 - t2
                      }
              pure (res, lp)
      else takeAvailableResource pool t1 lp stripe

-- | A variant of 'withResource' that doesn't execute the action and returns
-- 'Nothing' instead of blocking if the local pool is exhausted.
tryWithResource :: Pool a -> (Resource a -> IO r) -> IO (Maybe r)
tryWithResource pool act = mask $ \unmask ->
  tryTakeResource pool >>= \case
    Just (res, localPool) -> do
      r <- unmask (act res) `onException` destroyResource pool localPool (resource res)
      putResource localPool (resource res)
      pure (Just r)
    Nothing -> pure Nothing

-- | A variant of 'takeResource' that returns 'Nothing' instead of blocking if
-- the local pool is exhausted.
tryTakeResource :: Pool a -> IO (Maybe (Resource a, LocalPool a))
tryTakeResource pool = mask_ $ do
  t1 <- getMonotonicTime
  lp <- getLocalPool (localPools pool)
  join . atomically $ do
    stripe <- readTVar (stripeVar lp)
    if available stripe == 0
      then do
        writeTVar (stripeVar lp) stripe
        pure $ pure Nothing
      else fmap Just <$> takeAvailableResource pool t1 lp stripe

----------------------------------------
-- Helpers

takeAvailableResource
  :: Pool a
  -> Double
  -> LocalPool a
  -> Stripe a
  -> STM (IO (Resource a, LocalPool a))
takeAvailableResource pool t1 lp stripe = case cache stripe of
  [] -> do
    let newAvailable = available stripe - 1
    writeTVar (stripeVar lp) $! stripe {available = newAvailable}
    pure $ do
      t2 <- getMonotonicTime
      a <- createResource (poolConfig pool) `onException` restoreSize (stripeVar lp)
      t3 <- getMonotonicTime
      let res =
            Resource
              { resource = a
              , stripeNumber = stripeId lp
              , availableResources = newAvailable
              , acquisition = Immediate
              , acquisitionTime = t2 - t1
              , creationTime = Just $! t3 - t2
              }
      pure (res, lp)
  Entry a _ : as -> do
    let newAvailable = available stripe - 1
    writeTVar (stripeVar lp) $! stripe {available = newAvailable, cache = as}
    pure $ do
      t2 <- getMonotonicTime
      let res =
            Resource
              { resource = a
              , stripeNumber = stripeId lp
              , availableResources = newAvailable
              , acquisition = Immediate
              , acquisitionTime = t2 - t1
              , creationTime = Nothing
              }
      pure (res, lp)
