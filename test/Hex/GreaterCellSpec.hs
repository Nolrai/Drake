{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hex.GreaterCellSpec
  ( spec,
  )
where

import Control.Lens (Iso', Lens', from, (^.))
import Control.Lens.Properties (isIso, isLens, isSetter, isTraversal)
import Hex.Cell (Cell (Cell), cell, subcell, toCell)
import Hex.CellSpec ()
import Hex.Greater cell
import Hex.Direction (Direction)
import Hex.DirectionSpec ()
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function, property)
import Test.QuickCheck.Gen as QG
import Prelude

greaterCell' :: Iso' (Cell Bool, Cell Bool) (Greater cell Bool)
greaterCell' = greaterCell

greaterToSubcell' :: InsideOutside -> Direction -> Lens' (Greater cell Bool) Bool
greaterToSubcell' = greaterToSubcell

spec = do
  describe "greaterCell" . it "is an iso" . property $ isIso greaterCell'
  describe "greaterToSubcell" $ do
    it "is an setter" . property $ \insideOutside vn -> isSetter $ greaterToSubcell' insideOutside vn
    it "is an traversal" . property $ \insideOutside vn -> isTraversal $ greaterToSubcell' insideOutside vn
    it "is an lens" . property $ \insideOutside vn -> isLens $ greaterToSubcell' insideOutside vn

instance Arbitrary a => Arbitrary (Greater cell a) where
  arbitrary = do
    i <- arbitrary
    o <- arbitrary
    pure $ (i, o) ^. greaterCell

  shrink x = drop 1 $ do
    i <- shrink (x ^. inside)
    o <- shrink (x ^. outside)
    pure ((i, o) ^. greaterCell)

instance Arbitrary InsideOutside where
  arbitrary = QG.elements [Inside, Outside]

  shrink Inside = []
  shrink Outside = [Inside]

instance CoArbitrary a => CoArbitrary (Greater cell a)

instance Function a => Function (Greater cell a)
