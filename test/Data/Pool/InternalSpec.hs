{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Data.Pool.InternalSpec where

import Control.Concurrent
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Pool.Internal
import Data.Pool

spec :: Spec
spec = do
  describe "newPool" $ do
    it "does not throws an error if max resources is less than stripes" $ do
      newPool PoolConfig
        { createResource = pure ()
        , freeResource = \_ -> pure ()
        , poolCacheTTL = 60.0
        , poolMaxResources = 10
        , poolNumStripes = Nothing
        }
      pure ()
