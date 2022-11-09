{-# language OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module Main (main) where

import Hedgehog
import Hedgehog.Main
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Pool.Internal

main :: IO ()
main = do
  defaultMain [checkParallel $$(discover)]

inputs :: Gen Input
inputs = do
  resources <- Gen.int (Range.exponentialFrom 8 1 1000)
  stripes <- Gen.int (Range.exponentialFrom 20 1 100)
  pure $ Input resources stripes

prop_howManyResourcesPerStripe :: Property
prop_howManyResourcesPerStripe = property $ do
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

prop_resourcesPerStripe :: Property
prop_resourcesPerStripe = property $ do
  input <- forAll inputs
  let stripeAndResource = howManyStripes input
  let resourceAllocations = robin stripeAndResource
  sum resourceAllocations === inputMaxResources input
  diff (minimum resourceAllocations) (\a b -> abs (a - b) <= 1) (maximum resourceAllocations)
  length resourceAllocations === allowedStripes stripeAndResource
