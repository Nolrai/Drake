{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.VonNeumannSpec (spec) where

import STCA.VonNeumann (VonNeumann (..), allVonNeumann, inv, offset, rotateClockwise)
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function (..), functionMap, property)
import Test.QuickCheck.Gen as QG (elements)
import qualified Text.Show as T
import Prelude hiding ((^))
import qualified Prelude as P

-- force the types to match
(^) :: Integral a => a -> a -> a
n ^ m = n P.^ m

spec :: Spec
spec =
  do
    describe "allVonNeumann" $ do
      it "contains all the VonNeumann values once" . property $
        \vn -> length (filter (== vn) allVonNeumann) `shouldBe` 1
      it "has length 4" $ length allVonNeumann `shouldBe` 4
    describe "inv" $ do
      it "is involution" . property $
        \vn -> inv (inv vn) `shouldBe` vn
      it "is derangement" . property $
        \vn -> inv vn `shouldNotBe` vn
    describe "rotateClockwise" $ do
      it "is derangement" . property $
        \vn -> rotateClockwise vn `shouldNotBe` vn
      it "is a sqrt of inv" . property $
        \vn -> rotateClockwise (rotateClockwise vn) `shouldBe` inv vn
      it "takes N to E" . property $ rotateClockwise N `shouldBe` E
    describe "offset" $ do
      it "of inv is inverse of offset" . property $
        \(p :: (Int, Int)) vn -> offset (offset p vn) (inv vn) `shouldBe` p
      it "offsets a distance of 1" . property $
        let squared_distance (a, b) (c, d) = ((a - c) ^ 2 + (b - d) ^ 2 :: Int)
         in \p vn -> (p `squared_distance` offset p vn) `shouldBe` 1

instance Arbitrary VonNeumann where
  arbitrary = QG.elements allVonNeumann
  shrink N = []
  shrink E = [N]
  shrink S = [E, N]
  shrink W = [S, E, N]

instance CoArbitrary VonNeumann

instance Function VonNeumann

instance Eq a => Eq (VonNeumann -> a) where
  a == b = (a <$> allVonNeumann) == (b <$> allVonNeumann)

instance Show a => Show (VonNeumann -> a) where
  show f = T.show $ (\vn -> (vn, f vn)) <$> allVonNeumann

instance Function a => Function (VonNeumann -> a) where
  function = functionMap aToB bToA
    where
      aToB f = (f N, f E, f S, f W)

      bToA (n, _, _, _) N = n
      bToA (_, e, _, _) E = e
      bToA (_, _, s, _) S = s
      bToA (_, _, _, w) W = w
