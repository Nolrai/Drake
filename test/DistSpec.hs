{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module DistSpec
  ( spec,
  )
where

import Data.Set as Set
import Data.Map as Map
import Control.Monad (guard)
import Data.Vector as V
import Data.Histogram as H
import Dist
import Test.Hspec
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    NonZero (NonZero),
    Positive (Positive),
    Testable (property),
    (.&&.)
  )
import Prelude hiding (die)
import 

vectorToHistogram :: Ord a => Vector a -> Histogram a
vectorToHistogram = H.fromList a V.toList

dieSides :: Vector Char
dieSides = ['A'.. 'E']

die :: Dist Char
die = mkUniform . Set.fromAscList . V.toList $ dieSides

rootNumTrials = 100

testKey h key = (H.lookup key h `div` rootNumTrials) `shouldBe` (rootNumTrials `div` Prelude.length dieSides)

spec :: Spec
spec =
  do
    focus $ describe "drawFrom . mkUniform" $
      it "shouldbe aprox uniform" $
        do
          g <- getStdGen
          v <- V.replicateM (rootNumTrials * rootNumTrials) (g `drawFrom` die)
          let h = vectorToHistogram v
          V.mapM_ (testKey h) dieSides