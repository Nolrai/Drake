{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DistSpec
  ( spec,
  )
where

import Data.Histogram as Histogram
import Data.Set as Set
import Data.Vector as Vector
import Dist
import GHC.Real ((%))
import System.Random
import System.Random.Stateful
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (die)

vectorToHistogram :: Ord a => Vector a -> Histogram a
vectorToHistogram = Histogram.fromList . Vector.toList

dieSides :: Vector Char
dieSides = ['A' .. 'E']

die :: Dist Char
die = fromVector dieSides

rootNumTrials :: Int
rootNumTrials = 100

numTrials :: Int
numTrials = rootNumTrials * rootNumTrials

testKey :: Histogram Char -> Char -> Bool
testKey h key = (key, roundOff' (Histogram.lookup key h)) == (key, roundOff' (numTrials `div` Prelude.length dieSides))
  where
    roundOff' original = roundOff original rootNumTrials

roundOff :: Int -> Int -> Int
roundOff original scale = round (original % scale) * scale

isApproximatelyUniform :: Bool -> Dist Char -> Expectation
isApproximatelyUniform good dist =
  do
    g <- newAtomicGenM =<< getStdGen
    v <- Vector.replicateM numTrials (g `drawFrom` dist)
    let should = (if good then shouldSatisfy else shouldNotSatisfy) :: Histogram Char -> (Histogram Char -> Bool) -> Expectation
    vectorToHistogram v `should` \h -> Vector.all (testKey h) dieSides

spec :: Spec
spec = focus $
  do
    describe "isApproximatelyUniform" $ do
      it "should detect bad dice" . property $
        \l ->
          do
            let toDieSide a = dieSides Vector.! (abs a `mod` Vector.length dieSides)
            let v = Vector.fromList l
            let testDie = fromVector $ (toDieSide <$> v) <> dieSides
            testDie /= die ==> isApproximatelyUniform False testDie
    describe "drawFrom . fromVector" $ do
      it "shouldbe isApproximatelyUniform uniform" $
        isApproximatelyUniform True die
