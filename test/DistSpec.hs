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
import Test.QuickCheck hiding (scale)
import Control.Lens.Properties
import Control.Lens
import Prelude hiding (die)

vectorToHistogram :: Ord a => Vector a -> Histogram a
vectorToHistogram = Histogram.fromList . Vector.toList

dieSides :: Set Char
dieSides = Set.fromAscList ['A' .. 'E']

die :: Dist Char
die = fromSet dieSides

rootNumTrials :: Int
rootNumTrials = 100

numTrials :: Int
numTrials = rootNumTrials * rootNumTrials

testKey :: Histogram Char -> Char -> Bool
testKey h key = (key, roundOffTrials (Histogram.lookup key h)) == (key, roundOffTrials (numTrials `div` Prelude.length dieSides))

roundOffTrials :: Int -> Int
roundOffTrials original = roundOff original rootNumTrials

roundOff :: Int -> Int -> Int
roundOff original scale = round (original % scale) * scale

isApproximatelyUniform :: Bool -> Dist Char -> Expectation
isApproximatelyUniform good dist =
  do
    g <- newAtomicGenM =<< getStdGen
    v <- Vector.replicateM numTrials (g `drawFrom` dist)
    let should = (if good then shouldSatisfy else shouldNotSatisfy) :: Histogram Char -> (Histogram Char -> Bool) -> Expectation
    vectorToHistogram v `should` \h -> setAll (testKey h) dieSides

setAll :: (a -> Bool) -> Set a -> Bool
setAll p = Set.foldr (\ item sofar -> p item && sofar) True

spec :: Spec
spec = focus $
  do
    describe "isApproximatelyUniform" $ do
      it "should detect bad dice" . property $
        \l ->
          do
            let toDieSide a = chr $ ord (Set.findMin dieSides) + (abs a `mod` Set.size dieSides)
            let v = Vector.fromList l
            let testDie = fromVector $ (toDieSide <$> v) <> Vector.fromList (Set.toList dieSides)
            testDie /= die ==> isApproximatelyUniform False testDie
    describe "drawFrom . fromSet" $ do
      it " isApproximatelyUniform" $
        isApproximatelyUniform True die
    describe "simplify" $ do
      it "should produce eq Dist" . property $
        \ (d :: Dist Char) -> simplify d == d
    describe "convex" $ do
      it "is idempotent" . property $
        \ (d :: Dist Char) (Positive alpha) (Positive beta) -> convex alpha d beta d `shouldBe` d
      it "depnds only on the ratio" . property $
        \ (Positive alpha) (a :: Dist Char) (Positive beta) b (Positive gamma) -> 
          convex (gamma * alpha) a (gamma * beta) b `shouldBe` convex alpha a beta b
      it "on n 0 it = a" . property $
        \ (Positive alpha) (a :: Dist Char) b -> 
          convex alpha a 0 b `shouldBe` a
      it "on 0 n it = b" . property $
        \ (a :: Dist Char) (Positive beta) b -> 
          convex 1 a beta b `shouldBe` b
      it "is communative on scale/value pairs" . property $
        \ (Positive alpha) (a :: Dist Char) (Positive beta) b -> 
          convex alpha a beta b `shouldBe` convex beta b alpha a
      it "should result in a results ratio of alpha % beta" . property $
        \ (Positive alpha) (a :: Dist Char) (Positive beta) b -> 
          not (Dist.null a) && not (Dist.null b) ==>
            do
              let (combo :: Dist (Either Char Char)) = 
                    convex alpha (Left <$> a) beta (Right <$> b)
              g <- newAtomicGenM =<< getStdGen
              v <- Vector.replicateM numTrials (g `drawFrom` combo)
              let h = vectorToHistogram (isLeft <$> v)
              roundOffTrials (Histogram.lookup True h) `shouldBe` 
                roundOffTrials ((numTrials * alpha) `div` (alpha + beta))

    describe "histogram" $ do
      it "is an Iso" $ isIso (histogram :: Iso' (IntMap Char) (Map Char Int))