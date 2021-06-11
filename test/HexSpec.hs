{-# LANGUAGE NoImplicitPrelude #-}

module HexSpec
  ( spec,
  )
where

import Control.Lens (Lens', (^.))
import Control.Lens.Properties (isLens, isSetter, isTraversal)
import Drake (Torus, read2d)
import DrakeSpec ()
import Hex (greaterCellFromTorus, inside, subCellOfTorus, toggleSubCellOnTorus)
import Hex.Cell (Cell)
import Hex.CellSpec ()
import Hex.GreaterCell (GreaterCell)
import Hex.GreaterCellSpec ()
import Hex.Rules (RedBlack)
import Hex.RulesSpec ()
import Hex.DirectionSpec ()
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
