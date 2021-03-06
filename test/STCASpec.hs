{-# LANGUAGE NoImplicitPrelude #-}

module STCASpec
  ( spec,
  )
where

import Control.Lens (Lens', (^.))
import Control.Lens.Properties (isLens, isSetter, isTraversal)
import Drake (Torus, read2d)
import DrakeSpec ()
import STCA (greaterCellFromTorus, inside, subCellOfTorus, toggleSubCellOnTorus)
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
  describe "subCellOfTorus" $ do
    it "is an setter" . property $
      \pos vn -> isSetter (subCellOfTorus pos vn :: Lens' (Torus (Cell RedBlack)) RedBlack)
    it "is an traversal" . property $
      \pos vn -> isTraversal (subCellOfTorus pos vn :: Lens' (Torus (Cell RedBlack)) RedBlack)
    it "is an lens" . property $
      \pos vn -> isLens (subCellOfTorus pos vn :: Lens' (Torus (Cell RedBlack)) RedBlack)
  describe "greaterCellFromTorus" $ do
    it "is an setter" . property $ \pos -> isSetter (greaterCellFromTorus pos :: Lens' (Torus (Cell RedBlack)) (GreaterCell RedBlack))
    it "is an traversal" . property $ \pos -> isTraversal (greaterCellFromTorus pos :: Lens' (Torus (Cell RedBlack)) (GreaterCell RedBlack))
    it "is an lens" . property $ \pos -> isLens (greaterCellFromTorus pos :: Lens' (Torus (Cell RedBlack)) (GreaterCell RedBlack))
    it "is superset of read2d" . property $
      \pos t -> t ^. greaterCellFromTorus pos . inside == t ^. read2d pos
  describe "toggleSubCellOnTorus" $ do
    it "is involution" . property $
      \pos vn t -> toggleSubCellOnTorus pos vn (toggleSubCellOnTorus pos vn t) `shouldBe` t
    it "commutes" . property $
      \pos vn pos' vn' t -> toggleSubCellOnTorus pos' vn' (toggleSubCellOnTorus pos vn t) `shouldBe` toggleSubCellOnTorus pos vn (toggleSubCellOnTorus pos' vn' t)
