{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Data.Pool.InternalSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Pool.Internal

spec :: Spec
spec = do
  describe "howManyStripes" $ do
    it "has one stripe per resource" $ hedgehog $ do
      input@Input {..} <- forAll inputs
      let StripeResourceAllocation {allowedStripes = numStripes} = howManyStripes input
      -- actual stripes may be at most requested stripes
      if inputStripes >= inputMaxResources
        then do
          label "More stripes than resources"
        else do
          label "Fewer stripes than resources"
          inputStripes === numStripes
      diff numStripes (<=) inputStripes

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

inputs :: Gen Input
inputs = do
  resources <- Gen.int (Range.exponentialFrom 8 1 1000)
  stripes <- Gen.int (Range.exponentialFrom 20 1 100)
  pure $ Input resources stripes