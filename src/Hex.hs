{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA
  ( Direction (..),
    offset,
    Cell (),
    cell,
    subcell,
    toCell,
    subCellOfTorus,
    toggleSubCellOnTorus,
    GreaterCell,
    inside,
    outside,
    greaterCell,
    greaterToSubcell,
    greaterCellFromTorus,
    isLhzHead,
    lhzMap,
    RedBlack (..),
    allDirections,
    wideStep,
    TorusEx,
    torus,
    headSet,
    mkTorusEx,
    pairLens, -- should we really define this here??
  )
where

-- import Data.Set as Set

import Control.Arrow ((***))
import Control.Lens hiding (inside, outside, index)
import Data.Map as Map (fromList, keysSet, lookup)
import Data.Set as Set
import Data.Vector as Vector
import Drake (Torus, rangeT, read2d, rangeMod)
import Relude
import STCA.Cell (Cell (Cell), cell, subcell, toCell)
import STCA.GreaterCell
  ( GreaterCell (),
    greaterCell,
    greaterToSubcell,
    inside,
    outside,
  )
import STCA.Rules
  ( LhsTemplate,
    RedBlack (..),
    RhsTemplate,
    lhzBase,
    mkLHS,
    readBody,
    rotateLar,
    toBody,
    toHead,
    toggle,
    vnDiff,
  )
import STCA.Direction (Direction (..), allDirections, inv, offset)

-- given a x,y pair and a Direction direction access the sub cell of the cell at that index.
subCellOfTorus :: (Int, Int) -> Direction -> Lens' (Torus (Cell a)) a
subCellOfTorus pos vn = read2d pos . subcell vn

-- given thesame info as above, toggle the subcell from red to black or back again.
toggleSubCellOnTorus :: (Int, Int) -> Direction -> Torus (Cell RedBlack) -> Torus (Cell RedBlack)
toggleSubCellOnTorus pos vn = over (subCellOfTorus pos vn) toggle

-- A greaterCell is a cell and its sourounding sub-cells
greaterCellFromTorus :: (Int, Int) -> Lens' (Torus (Cell RedBlack)) (GreaterCell RedBlack)
greaterCellFromTorus pos = pairLens readInside readOutside . greaterCell
  where
    readInside :: ALens' (Torus (Cell RedBlack)) (Cell RedBlack)
    readInside = read2d pos
    readOutside :: ALens' (Torus (Cell RedBlack)) (Cell RedBlack)
    readOutside = sequenceL cellOfLenses
    cellOfLenses :: Cell (ALens' (Torus (Cell RedBlack)) RedBlack)
    cellOfLenses = fromVN ^. toCell
    fromVN :: Direction -> ALens' (Torus (Cell RedBlack)) RedBlack
    fromVN vn = read2d (offset pos vn) . subcell (inv vn)

-- Given two lenses from the same type, make a lense from that type to the pair of them
-- the order can matter, the second lens's put can overwrite the first's.
pairLens :: forall t a b. ALens' t a -> ALens' t b -> Lens' t (a, b)
pairLens lens1 lens2 = lens get' put'
  where
    get' t = (t ^# lens1, t ^# lens2)
    put' t (v1, v2) = storing lens2 v2 (storing lens1 v1 t)

-- Turn a cell of lenses into a lense to a cell.
-- this is named after sequenceM's behavior on lists
sequenceL :: forall t a. Cell (ALens' t a) -> ALens' t (Cell a)
sequenceL (Cell (n, e, s, w)) = lens get_ put_
  where
    get_ :: t -> Cell a
    get_ t = Cell (t ^. cloneLens n, t ^. cloneLens e, t ^. cloneLens s, t ^. cloneLens w)
    put_ :: t -> Cell a -> t
    put_ t (Cell (n', e', s', w')) =
      set
        (cloneLens n)
        n'
        ( set
            (cloneLens e)
            e'
            ( set
                (cloneLens s)
                s'
                ( set
                    (cloneLens w)
                    w'
                    t
                )
            )
        )

lhsToTemplate :: LhsTemplate -> GreaterCell RedBlack
lhsToTemplate lhs =
  (inside' ^. toCell, outside' ^. toCell) ^. greaterCell
  where
    inside', outside' :: Direction -> RedBlack
    -- the inside is filled where the template is Black
    inside' vn = maybe Red (\lar -> lhs ^. toBody . readBody lar) ((lhs ^. toHead) `vnDiff` vn)
    outside' vn = if lhs ^. toHead == vn then Black else inside' vn -- the outside is also filled in at the head

rhsToTemplate :: Direction -> RhsTemplate -> GreaterCell RedBlack
rhsToTemplate old_head rhs =
  (inside' ^. toCell, outside' ^. toCell) ^. greaterCell
  where
    new_head :: Direction
    new_head = (rhs ^. toHead) `rotateLar` old_head
    -- the outside is filled only where the template is Black
    outside', inside' :: Direction -> RedBlack
    outside' vn = maybe Red (\lar -> rhs ^. toBody . readBody lar) (new_head `vnDiff` vn)
    -- the inside also filled in at the head the head
    inside' vn = if new_head == vn then Black else outside' vn

lhzMap :: Map (GreaterCell RedBlack) (GreaterCell RedBlack, Direction)
lhzMap = Map.fromList lhzList

-- for testing
lhzList :: [(GreaterCell RedBlack, (GreaterCell RedBlack, Direction))]
lhzList = do
  vn <- allDirections
  (l, r) <- lhzBase
  pure (lhsToTemplate (mkLHS vn l), (rhsToTemplate vn r, (r ^. toHead) `rotateLar` vn))

data TorusEx = TorusEx {_torus :: Torus (Cell RedBlack), _headSet :: Set (Int, Int)}

makeLenses ''TorusEx

wideStep :: TorusEx -> Int -> TorusEx
wideStep oldState index = if Set.null headSet' then error $ fromString "headSet is null" else applyRule oldState pos
  where
    headSet' = oldState ^. headSet
    pos = (Vector.fromList . Set.toList) headSet' ! (index `rangeMod` Set.size headSet') 

applyRule :: TorusEx -> (Int, Int) -> TorusEx
-- pos is the index of the cell the head is pointing into
applyRule old pos =
  (torus %~ modifyTorus_) . (headSet %~ modifyHeadSet_) $ old
  where
    modifyTorus_ :: Torus (Cell RedBlack) -> Torus (Cell RedBlack)
    modifyHeadSet_ :: Set (Int, Int) -> Set (Int, Int)
    (modifyTorus_, modifyHeadSet_) =
      maybe
        (id, Set.delete pos)
        (applyRuleResult pos)
        (old ^. lookupGreaterCell pos :: Maybe (GreaterCell RedBlack, Direction))

applyRuleResult ::
  (Int, Int) ->
  (GreaterCell RedBlack, Direction) ->
  (Torus (Cell RedBlack) -> Torus (Cell RedBlack), Set (Int, Int) -> Set (Int, Int))
applyRuleResult pos = applyRuleToTorus pos *** applyRuleToHeadSet pos

lookupGreaterCell :: (Int, Int) -> Getter TorusEx (Maybe (GreaterCell RedBlack, Direction))
lookupGreaterCell pos = torus . greaterCellFromTorus pos . to (`Map.lookup` lhzMap)

isLhzHead :: (Int, Int) -> Getter TorusEx Bool
isLhzHead pos = lookupGreaterCell pos . to isJust

applyRuleToTorus :: (Int, Int) -> GreaterCell RedBlack -> Torus (Cell RedBlack) -> Torus (Cell RedBlack)
applyRuleToTorus pos newGC = greaterCellFromTorus pos .~ newGC

applyRuleToHeadSet :: (Int, Int) -> Direction -> Set (Int, Int) -> Set (Int, Int)
applyRuleToHeadSet pos newHead = Set.insert (pos `offset` newHead) . Set.delete pos

mkTorusEx :: Torus (Cell RedBlack) -> TorusEx
mkTorusEx t =
  TorusEx
    { _torus = t,
      _headSet =
        Set.fromAscList . Vector.toList . Vector.filter isActiveCell $ rangeT t
    }
  where
    isActiveCell pos = (t ^. greaterCellFromTorus pos) `Set.member` Map.keysSet lhzMap
