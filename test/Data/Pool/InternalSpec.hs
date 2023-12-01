{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Data.Pool.InternalSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe (fromMaybe)
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Pool.Internal
import Data.Pool

spec :: Spec
spec = do
  describe "newPool" $ do
    it "throws an error if max resources is less than stripes" $ hedgehog $ do
      maxResources <- forAll $ Gen.integral (Range.linear 1 100)
      numStripes <- forAll $ Gen.maybe $ Gen.integral (Range.linear 1 100)
      nothingStripes <- evalIO $ getNumCapabilities
      eresult <- fmap void $ evalIO $ try $ newPool PoolConfig
        { createResource = pure ()
        , freeResource = \_ -> pure ()
        , poolCacheTTL = 60.0
        , poolMaxResources = maxResources
        , poolNumStripes = numStripes
        }
      let actualStripes = fromMaybe nothingStripes numStripes
      classify ("maxResources < actualStripes") (maxResources < actualStripes)
      case eresult of
        Left (ErrorCall msg) -> do
          diff maxResources (<) actualStripes
          msg === "poolMaxResources must not be smaller than numStripes"
        Right _ ->
          pure ()

  describe "poolAvailableResources" $ do
    let
      mkPool =
        newPool PoolConfig
          { createResource = pure ()
          , freeResource = \_ -> pure ()
          , poolCacheTTL = 60.0
          , poolMaxResources = 10
          , poolNumStripes = Just 1
          }
    before mkPool $ do
      it "works" $ \pool -> do
        poolAvailableResources pool
          `shouldReturn`
            10
      it "works when a resource has been taken" $ \pool -> do
        withResource pool $ \_ ->
          poolAvailableResources pool
            `shouldReturn`
              9
      it "nested withResource works fine too" $ \pool -> do
        withResource pool $ \_ -> do
          poolAvailableResources pool
            `shouldReturn`
              9
          withResource pool $ \_ -> do
            poolAvailableResources pool
              `shouldReturn`
                8
