{-# LANGUAGE NoImplicitPrelude #-}

module STCASpec
  ( spec,
  )
where

import Control.Lens (Lens')
import Control.Lens.Properties (isLens)
import Drake (Torus)
import DrakeSpec ()
import STCA (greaterCell, greaterCellFromTorus, subCellOfTorus, toggleCellOnTorus)
import STCA.Cell (Cell)
import STCA.CellSpec ()
import STCA.GreaterCell (GreaterCell)
import STCA.GreaterCellSpec ()
import STCA.Rules (RedBlack)
import STCA.RulesSpec ()
import STCA.VonNeumannSpec ()
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Prelude

spec :: Spec
spec = do
  describe "subCellOfTorus" $
    it "is an lens" . property $
      \pos vn -> isLens (subCellOfTorus pos vn :: Lens' (Torus (Cell RedBlack)) RedBlack)
  describe "greaterCellFromTorus" $
    it "is an lens" . property $
      \pos -> isLens (greaterCellFromTorus pos :: Lens' (Torus (Cell RedBlack)) (GreaterCell RedBlack))
  describe "toggleCellOnTorus" $ do
    it "is involution" . property $
      \pos vn t -> toggleCellOnTorus pos vn (toggleCellOnTorus pos vn t) `shouldBe` t
    it "commutes" . property $
      \pos vn pos' vn' t -> toggleCellOnTorus pos' vn' (toggleCellOnTorus pos vn t) `shouldBe` toggleCellOnTorus pos vn (toggleCellOnTorus pos' vn' t)
