{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hex
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
    ruleMap,
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
import qualified Data.Map.ToMonoid as MMap (keysSet, lookup)
import Data.Set as Set
import Data.Vector as Vector
import Drake (Torus, rangeT, read2d, rangeMod)
import Relude
import Hex.Cell (Cell (Cell), cell, subcell, toCell)
import Hex.GreaterCell
  ( GreaterCell (),
    greaterCell,
    greaterToSubcell,
    inside,
    outside,
  )
import Hex.Rules
import Hex.Direction (Direction (..), allDirections, inv, offset)

-- given a x,y pair and a Direction access the sub cell of the cell at that index.
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
sequenceL :: forall t a. Cell (ALens' t a) -> Lens' t (Cell a)
sequenceL (Cell (yz, xz, xy, zy, zx, yx)) = split' . cellify
  where
    split :: Lens' t (a,(a,(a,(a,(a,a)))))
    split = 
      pairLenses yz . pairLenses xz . pairLenses xy . pairLenses zy . pairLenses zx . pairLenses yx
    cellify :: Iso' (a,(a,(a,(a,(a,a))))) (Cell a)
    cellify = iso f g
    f (yx',(xz',(xy',(zy',(zx',yx'))))) = cell ell yx' xz' xy' zy' zx' yx'
    g (Cell (yz', xz', xy', zy', zx', yx')) = (yx',(xz',(xy',(zy',(zx',yx')))))

data TorusEx = TorusEx {_torus :: Torus (Cell RedBlack), _headMap :: Map (Int, Int) Direction}

makeLenses ''TorusEx

asLHS, asRHS :: Prism' (GreaterCell RedBlack) (Cell (Maybe RedBlack)) -- "Nothing" means a head
asLHS = prism' _put _test
  where
    _put c =
      let f def = (\ dir -> fromMaybe def (c ^. subcell dir) ) 
      in (f Red, f Black) ^. from greaterCell
    _test gc = executingStateT (cell Nothing Nothing Nothing Nothing Nothing Nothing) $
        allDirections `forM_` 
          \ dir -> 
            do
            let f io = gc ^. greaterToSubcell io dir
            let (i,o) = (f Inside, f Outside)
            r <- 
              if i == o 
                then pure (Just i)
                else if o == Black 
                  then pure Nothing
                  else lift Nothing
            subcell .= r

asRHS = eversion . asLHS

rhsBody c dir = 
  let (h, b) = c ^. cellToBody dir
  in if isNothing h
    then Just (fromMaybe Red <$> b)
    else Nothing

eversion :: Iso (GreaterCell a) (GreaterCell a)
eversion = from greaterCell . swap . greaterCell
  where
    swap (i,o) = (o,i)







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
lookupGreaterCell pos = torus . greaterCellFromTorus pos . to (`Map.lookup` ruleMap)

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
    isActiveCell pos = (t ^. greaterCellFromTorus pos) `Set.member` Map.keysSet ruleMap
