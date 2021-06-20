{-# LANGUAGE NoImplicitPrelude #-}

module SquareSpec
  ( spec,
  )
where

import Control.Lens (Lens', (^.))
import Control.Lens.Properties (isLens, isSetter, isTraversal)
import Drake (Torus, read2d)
import DrakeSpec ()
import Square (greaterCellFromTorus, inside, subCellOfTorus, toggleSubCellOnTorus)
import Square.Cell (Cell)
import Square.CellSpec ()
import Square.Greater cell (Greater cell)
import Square.GreaterCellSpec ()
import Square.Rules (RedBlack)
import Square.RulesSpec ()
import Square.DirectionSpec ()
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
    it "is an setter" . property $ \pos -> isSetter (greaterCellFromTorus pos :: Lens' (Torus (Cell RedBlack)) (Greater cell RedBlack))
    it "is an traversal" . property $ \pos -> isTraversal (greaterCellFromTorus pos :: Lens' (Torus (Cell RedBlack)) (Greater cell RedBlack))
    it "is an lens" . property $ \pos -> isLens (greaterCellFromTorus pos :: Lens' (Torus (Cell RedBlack)) (Greater cell RedBlack))
    it "is superset of read2d" . property $
      \pos t -> t ^. greaterCellFromTorus pos . inside == t ^. read2d pos
  describe "toggleSubCellOnTorus" $ do
    it "is involution" . property $
      \pos vn t -> toggleSubCellOnTorus pos vn (toggleSubCellOnTorus pos vn t) `shouldBe` t
    it "commutes" . property $
      \pos vn pos' vn' t -> toggleSubCellOnTorus pos' vn' (toggleSubCellOnTorus pos vn t) `shouldBe` toggleSubCellOnTorus pos vn (toggleSubCellOnTorus pos' vn' t)
