module Data.PoolSpec where

import Control.Monad
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Pool

spec :: Spec
spec = do
  describe "createPool" $ do
    it "does not error for any legal set of inputs" $ hedgehog $ do
      numStripes <- forAll $ Gen.integral $ Range.linear 1 100
      idleTime <- forAll $ Gen.realFrac_ $ Range.linearFrac 0.5 100
      maxResources <- forAll $ Gen.integral $ Range.linear 1 100
      void $  evalIO $ createPool (pure ()) (\_ -> pure ()) numStripes idleTime maxResources
