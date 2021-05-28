{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA
  ( VonNeumann (..),
    offset,
    Cell (),
    cell,
    subcell,
    toCell,
    subCellOfTorus,
    toggleCellOnTorus,
    GreaterCell,
    inside,
    outside,
    greaterCell,
    readGreaterCell,
    greaterCellFromTorus,
    lhzMap,
    findHeadCells,
    RedBlack (..),
    allVonNeumann,
  )
where

-- import Data.Set as Set

import Control.Lens hiding (inside, outside)
import Data.Map as M (fromList)
import Data.Vector as V
import Drake (Torus, rangeT, read2d)
import Relude
  ( Applicative (pure),
    Eq ((==)),
    Int,
    Map,
    guard,
    maybe,
    (&&),
    (.),
  )
import STCA.Cell (Cell (Cell), cell, subcell, toCell)
import STCA.GreaterCell
  ( GreaterCell (..),
    InsideOutside (Inside, Outside),
    greaterCell,
    inside,
    outside,
    readGreaterCell,
  )
import STCA.Rules
  ( LAR,
    LhsTemplate (..),
    RedBlack (..),
    RhsTemplate (..),
    lhzBase,
    readBody,
    rotateLar,
    toggle,
    vnDiff,
  )
import STCA.VonNeumann (VonNeumann (..), allVonNeumann, inv, offset)

subCellOfTorus :: (Int, Int) -> VonNeumann -> Lens' (Torus (Cell a)) a
subCellOfTorus pos vn = read2d pos . subcell vn

toggleCellOnTorus :: (Int, Int) -> VonNeumann -> Torus (Cell RedBlack) -> Torus (Cell RedBlack)
toggleCellOnTorus pos vn = over (subCellOfTorus pos vn) toggle

-- A greaterCell is a cell and its sourounding sub-cells
greaterCellFromTorus :: (Int, Int) -> Lens' (Torus (Cell RedBlack)) (GreaterCell RedBlack)
greaterCellFromTorus pos = splitLens readInside readOutside . greaterCell
  where
    readInside :: ALens' (Torus (Cell RedBlack)) (Cell RedBlack)
    readInside = read2d pos
    readOutside :: ALens' (Torus (Cell RedBlack)) (Cell RedBlack)
    readOutside = sequenceL cellOfLenses
    cellOfLenses :: Cell (ALens' (Torus (Cell RedBlack)) RedBlack)
    cellOfLenses = fromVN ^. toCell
    fromVN :: VonNeumann -> ALens' (Torus (Cell RedBlack)) RedBlack
    fromVN vn = read2d (offset pos (inv vn)) . subcell vn

splitLens :: forall t a b. ALens' t a -> ALens' t b -> Lens' t (a, b)
splitLens lens1 lens2 = lens get put
  where
    get t = (t ^# lens1, t ^# lens2)
    put t (v1, v2) = storing lens2 v2 (storing lens1 v1 t)

sequenceL :: forall t a. Cell (ALens' t a) -> ALens' t (Cell a)
sequenceL (Cell (n, e, s, w)) = lens get put
  where
    get :: t -> Cell a
    get t = Cell (t ^. cloneLens n, t ^. cloneLens e, t ^. cloneLens s, t ^. cloneLens w)
    put t (Cell (n', e', s', w')) = set (cloneLens n) n' (set (cloneLens e) e' (set (cloneLens s) s' (set (cloneLens w) w' t)))

findHeadCells :: Torus (Cell RedBlack) -> Vector ((Int, Int), VonNeumann)
findHeadCells tz =
  do
    pos <- rangeT tz
    let t = readGreaterCell (tz ^. greaterCellFromTorus pos)
    vn <- V.fromList allVonNeumann
    guard (t Inside vn == Red && t Outside vn == Black)
    pure (pos, vn)

lhsToTemplate :: LhsTemplate -> GreaterCell RedBlack
lhsToTemplate LHS {..} =
  (inside' ^. toCell, outside' ^. toCell) ^. greaterCell
  where
    inside', outside' :: VonNeumann -> RedBlack
    -- the inside is filled where the template is Black
    inside' vn = maybe Red (lhsBody `readBody`) (lhsHead `vnDiff` vn)
    outside' vn = if lhsHead == vn then Black else inside' vn -- the outside is also filled in at the head

rhsToTemplate :: VonNeumann -> RhsTemplate -> GreaterCell RedBlack
rhsToTemplate old_head RHS {..} =
  (inside' ^. toCell, outside' ^. toCell) ^. greaterCell
  where
    new_head :: VonNeumann
    new_head = rhs_head `rotateLar` old_head
    -- the outside is filled only where the template is Black
    outside', inside' :: VonNeumann -> RedBlack
    outside' vn = maybe Red (rhs_body `readBody`) (new_head `vnDiff` vn)
    -- the inside also filled in at the head the head
    inside' vn = if new_head == vn then Black else outside' vn

lhzMap :: Map (GreaterCell RedBlack) (GreaterCell RedBlack, LAR)
lhzMap =
  M.fromList
    ( do
        vn <- allVonNeumann
        (l, r) <- lhzBase
        pure (lhsToTemplate (LHS vn l), (rhsToTemplate vn r, rhs_head r))
    )
