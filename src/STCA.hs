{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA
  ( VonNeumann (..),
    offset,
    Cell (),
    cell,
    readCell,
    writeCell,
    toCell,
    writeCellOnTorus,
    toggleCellOnTorus,
    GreaterCell (),
    template,
    readTemplate,
    readTemplateFromTorus,
    lhzMap,
    findHeadCells,
    RedBlack (..),
    allVonNeuman,
  )
where

-- import Data.Set as Set
import Data.Map as M (fromList)
import Drake (TorusZipper, rangeT, read2d, write2d)
import Relude
  ( Applicative (pure),
    Eq ((==)),
    Int,
    Map,
    guard,
    maybe,
    (&&),
    (<$>),
  )
import STCA.Cell (Cell, cell, readCell, toCell, writeCell)
import STCA.GreaterCell
  ( GreaterCell (..),
    InsideOutside (Inside, Outside),
    readTemplate,
    template,
  )
import STCA.Rules
  ( LAR,
    LhsTemplate (..),
    RedBlack (..),
    RhsTemplate (..),
    lhzBase,
    readBody,
    rotateLar,
    vnDiff,
  )
import STCA.VonNeumann (VonNeumann (..), allVonNeuman, inv, offset)

writeCellOnTorus :: (Int, Int) -> VonNeumann -> a -> TorusZipper (Cell a) -> TorusZipper (Cell a)
writeCellOnTorus pos vn value tz = write2d tz pos (writeCell (tz `read2d` pos) vn value)

toggleCellOnTorus :: (Int, Int) -> VonNeumann -> TorusZipper (Cell RedBlack) -> TorusZipper (Cell RedBlack)
toggleCellOnTorus pos vn tz =
  let targetCell = tz `read2d` pos
   in write2d tz pos (writeCell targetCell vn (targetCell `readCell` vn))

readTemplateFromTorus :: forall a. TorusZipper (Cell a) -> (Int, Int) -> GreaterCell a
readTemplateFromTorus tz pos =
  GreaterCell (tz `read2d` pos, readOffset <$> cell N E S W)
  where
    readOffset :: VonNeumann -> a
    readOffset vn = (tz `read2d` offset pos vn) `readCell` inv vn

findHeadCells :: TorusZipper (Cell RedBlack) -> [((Int, Int), VonNeumann)]
findHeadCells tz =
  do
    pos <- rangeT tz
    let t = readTemplate (tz `readTemplateFromTorus` pos)
    vn <- [N, E, S, W]
    guard (t Inside vn == Red && t Outside vn == Black)
    pure (pos, vn)

lhsToTemplate :: LhsTemplate -> GreaterCell RedBlack
lhsToTemplate LHS {..} =
  template (toCell inside) (toCell outside)
  where
    -- the inside is filled where the template is Black
    inside vn = maybe Red (lhsBody `readBody`) (lhsHead `vnDiff` vn)
    outside vn = if lhsHead == vn then Black else inside vn -- the outside is also filled in at the head

rhsToTemplate :: VonNeumann -> RhsTemplate -> GreaterCell RedBlack
rhsToTemplate old_head RHS {..} =
  template (toCell inside) (toCell outside)
  where
    new_head :: VonNeumann
    new_head = rhs_head `rotateLar` old_head
    -- the outside is filled only where the template is Black
    outside, inside :: VonNeumann -> RedBlack
    outside vn = maybe Red (rhs_body `readBody`) (new_head `vnDiff` vn)
    -- the inside also filled in at the head the head
    inside vn = if new_head == vn then Black else outside vn

lhzMap :: Map (GreaterCell RedBlack) (GreaterCell RedBlack, LAR)
lhzMap =
  M.fromList
    ( do
        vn <- allVonNeuman
        (l, r) <- lhzBase
        pure (lhsToTemplate (LHS vn l), (rhsToTemplate vn r, rhs_head r))
    )
