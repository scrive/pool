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
  describe "howManyStripes" $ do
    it "has one stripe per resource" $ hedgehog $ do
      input@Input {..} <- forAll inputs
      let allocation = howManyStripes input
      -- actual stripes may be at most requested stripes
      if inputStripes >= inputMaxResources
        then do
          label "More stripes than resources"
        else do
          label "Fewer stripes than resources"
          inputStripes === allowedStripes allocation
      diff (allowedStripes allocation) (<=) inputStripes

  describe "robin" $ do
    it "sum of resources is always equal to input" $ hedgehog $ do
      input <- forAll inputs
      let resourceAllocations = robin (howManyStripes input)
      sum resourceAllocations === inputMaxResources input
    it "the difference between smallest and largest stripe is at most 1" $ hedgehog $ do
      input <- forAll inputs
      let resourceAllocations = robin (howManyStripes input)
      diff (minimum resourceAllocations) (\a b -> abs (a - b) <= 1) (maximum resourceAllocations)
    it "there is a resource allocation for each stripe" $ hedgehog $ do
      input <- forAll inputs
      let stripeAndResource = howManyStripes input
      let resourceAllocations = robin stripeAndResource
      length resourceAllocations === allowedStripes stripeAndResource

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

  describe "poolAvaialableResources" $ do
    let
      maxResources =
        10
      mkPool =
        newPool PoolConfig
          { createResource = pure ()
          , freeResource = \_ -> pure ()
          , poolCacheTTL = 60.0
          , poolMaxResources = maxResources
          , poolNumStripes = Just 10
          }
    before mkPool $ do
      it "reports the max when nothing is taken" $ \pool -> do
        poolAvailableResources pool
          `shouldReturn` 10
      it "has one fewer when a resource is taken" $ \pool -> do
        withResource pool $ \() ->
          poolAvailableResources pool
            `shouldReturn` 9
        poolAvailableResources pool
          `shouldReturn` 10

      it "works with nesting" $ \pool -> do
        print =<< getNumCapabilities
        withResource pool $ \() -> do
          poolAvailableResources pool
            `shouldReturn` 9
          withResource pool $ \() -> do
            poolAvailableResources pool
              `shouldReturn` 8
        poolAvailableResources pool
          `shouldReturn` 10


inputs :: Gen Input
inputs = do
  resources <- Gen.int (Range.exponentialFrom 8 1 1000)
  stripes <- Gen.int (Range.exponentialFrom 20 1 100)
  pure $ Input resources stripes
