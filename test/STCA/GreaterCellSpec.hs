{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.GreaterCellSpec
  ( spec,
  )
where

import STCA.GreaterCell
import Control.Lens (Lens', from, (^.))
import Control.Lens.Properties (isLens, isSetter, isTraversal)
import STCA.Cell (Cell (Cell), cell, subcell, toCell)
import STCA.VonNeumann (VonNeumann)
import STCA.VonNeumannSpec ()
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function, property)
import Test.QuickCheck.Gen as QG
import Prelude

greaterCell' :: Iso (GreaterCell Bool) (Cell Bool, Cell Bool)
greaterCell' = greaterCell

greaterToSubcell' :: InsideOutside -> VonNeumann -> Lens (GreaterCell Bool) Bool
greaterToSubcell' = greaterToSubcell

spec = do
  describe "greaterCell" . it "is a lens" . property $ isIso greaterCell'
  describe "greaterToSubcell" . it "is a lens" . property $ \ insideOutside vn -> isLens $ greaterToSubcell' insideOutside vn

instance Arbitrary a => Arbitrary (GreaterCell a) where
  arbitrary = do
    i <- arbitary
    o <- arbitary
    pure (view greaterCell (i,o))

  shrink x = drop 1 $ do
    i <- shrink (x ^. inside)
    o <- shrink (x ^. outside)
    pure ((i,o) ^. greaterCell)

instance Arbitrary InsideOutside where
  arbitary = QG.elements [Inside, Outside]
  shrink Inside = []
  shrink Outside = [Inside]