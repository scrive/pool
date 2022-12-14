{-# language OverloadedStrings #-}

module Data.PoolSpec where

import Control.Monad
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Pool
import System.Timeout
import Control.Concurrent

spec :: Spec
spec = do
  describe "createPool" $ do
    it "does not error for any legal set of inputs" $ hedgehog $ do
      numStripes <- forAll $ Gen.integral $ Range.linear 1 100
      idleTime <- forAll $ Gen.realFrac_ $ Range.linearFrac 0.5 100
      maxResources <- forAll $ Gen.integral $ Range.linear 1 100
      void $  evalIO $ createPool (pure ()) (\_ -> pure ()) numStripes idleTime maxResources


  describe "timeout functions" $ do
    let mkPool =
          newPool PoolConfig
            { createResource = threadDelay createDelay
            , freeResource = mempty
            , poolCacheTTL = 0.5
            , poolMaxResources = 4
            , poolNumStripes = Just 4
            }
        createDelay =
          100000

    describe "takeResourceWithTimeout" $ before mkPool $ do
      it "respects timeout" $ \pool -> hedgehog $ do
        -- pool <- evalIO mkPool
        overOrUnder <- forAll $ Gen.enum Double Half
        collect overOrUnder
        let timeoutDelay = doubleOrHalf overOrUnder createDelay
        munit <- fmap (fmap fst) $ evalIO $ timeout timeoutDelay $ takeResource pool
        case overOrUnder of
          Double ->
            munit === Just ()
          Half ->
            -- munit may be Just () if the pool had resources free
            munit === Nothing

    describe "withResourceTimeout" $ before mkPool $ do
      it "respects timeout" $ \pool -> hedgehog $ do
        pool' <- evalIO mkPool
        overOrUnder <- forAll $ Gen.enum Double Half
        collect overOrUnder
        let timeoutDelay = doubleOrHalf overOrUnder createDelay
        munit <- evalIO $ withResourceTimeout timeoutDelay pool pure
        case overOrUnder of
          Double ->
            munit === Just ()
          Half ->
            -- munit may be Just () if the pool had resources free
            munit === Nothing

data DoubleOrHalf = Double | Half
  deriving (Show, Eq, Ord, Enum, Bounded)

doubleOrHalf :: DoubleOrHalf -> Int -> Int
doubleOrHalf x = case x of
  Double -> (* 2)
  Half -> (`div` 2)
