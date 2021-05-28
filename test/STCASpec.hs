{-# LANGUAGE NoImplicitPrelude #-}

module STCASpec
  ( spec,
  )
where

import Control.Lens ()
import Control.Lens.Properties (isLens)
import DrakeSpec
import STCA (greaterCell, greaterCellFromTorus, subCellOfTorus, toggleCellOnTorus)
import STCA.CellSpec ()
import STCA.VonNeumannSpec ()
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Prelude

spec :: Spec
spec = do
  describe "subCellOfTorus" $
    it "is an lens" . property $
      \pos vn -> isLens (subCellOfTorus pos vn)
  describe "greaterCellFromTorus" $
    it "is an lens" . property $
      \pos -> isLens (greaterCellFromTorus pos)
  describe "toggleCellOnTorus" $ do
    it "is involution" . property $
      \pos vn t -> toggleCellOnTorus pos vn (toggleCellOnTorus pos vn t) `shouldBe` t
    it "commutes" . property $
      \pos vn pos' vn' t -> toggleCellOnTorus pos' vn' (toggleCellOnTorus pos vn t) `shouldBe` toggleCellOnTorus pos vn (toggleCellOnTorus pos' vn' t)
  describe "greaterCell" $
    it "is an lens" . property $ isLens greaterCell
