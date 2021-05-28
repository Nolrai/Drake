{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}

module STCA.CellSpec
  ( spec,
  )
where

import STCA.Cell (Cell(Cell), cell, subcell, toCell)
import Control.Lens
import Data.Text ()
import Test.Hspec
import Test.Hspec.Golden ()
import Test.QuickCheck (Arbitrary(..), property)
import Prelude
import Control.Lens.Properties
import STCA.VonNeumann (VonNeumann)

subcell' :: VonNeumann -> Lens' (Cell Bool) Bool
subcell' = subcell

spec :: Spec
spec =
  describe "Cell" $ do
    describe "toCell" $
      it "is an iso" . property $ isIso toCell
    describe "subcell" $ do
      it "is an setter" . property $ \ nv -> isSetter $ subcell' nv
      it "is an traversal" . property $ \ nv -> isTraversal $ subcell' nv
      it "is an lens" . property $ \ nv -> isLens $ subcell' nv
      
instance Arbitrary a => Arbitrary (Cell a) where
  arbitrary = cell <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Cell (n, e, s, w)) = 
    drop 1 $ cell <$> n : shrink n <*> e : shrink e <*> s : shrink s <*> w : shrink w