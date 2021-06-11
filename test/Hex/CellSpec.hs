{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hex.CellSpec
  ( spec,
  )
where

import Control.Lens (Lens', from, (^.))
import Control.Lens.Properties (isLens, isSetter, isTraversal)
import Hex.Cell (Cell (Cell), cell, subcell, toCell)
import Hex.Direction (Direction)
import Hex.DirectionSpec ()
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function, property)
import Prelude

-- instance of the Arbitrary typeclass for Direction .

subcell' :: Direction -> Lens' (Cell Bool) Bool
subcell' = subcell

spec :: Spec
spec =
  describe "Cell" $ do
    describe "toCell is an iso:" $ do
      it "cell to function to cell cancels" . property $
        \(c :: Cell Int) -> c ^. (from toCell . toCell) `shouldBe` c
      it "to function to cell to function cancels" . property $
        \(f :: Direction -> Int) -> f ^. (toCell . from toCell) `shouldBe` f
    describe "subcell" $ do
      it "is an setter" . property $ \vn -> isSetter $ subcell' vn
      it "is an traversal" . property $ \vn -> isTraversal $ subcell' vn
      it "is an lens" . property $ \vn -> isLens $ subcell' vn

instance Arbitrary a => Arbitrary (Cell a) where
  arbitrary = cell <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Cell (n, e, s, w)) =
    drop 1 $ cell <$> n : shrink n <*> e : shrink e <*> s : shrink s <*> w : shrink w

instance Function a => Function (Cell a)

instance CoArbitrary a => CoArbitrary (Cell a)
