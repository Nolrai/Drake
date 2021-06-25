{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Hex
  ( Direction (..),
    offset,
    Cell (),
    cell,
    subcell,
    toCell,
    subCellOfTorus,
    toggleSubCellOnTorus,
    Greater,
    inside,
    outside,
    greaterCell,
    greaterToSubcell,
    greaterCellFromTorus,
    isLhzHead,
    ruleVector,
    RedBlack (..),
    allDirections,
    wideStep,
    TorusEx,
    torus,
    headSet,
    mkTorusEx,
    pairLenses, -- should we really define this here??
  )
where

-- import Data.Set as Set

import Control.Arrow ((***))
import Control.Lens hiding (inside, outside, index)
import Data.Map as Map (lookup)
import Data.Set as Set
import Data.Vector as Vector
import Drake (Torus, rangeT, read2d, rangeMod)
import Relude
import Hex.Cell (Cell (Cell), cell, subcell, toCell)
import Hex.Rules
import Hex.Direction (Direction (..), allDirections, inv, offset)
import Greater

-- given a x,y pair and a Direction access the sub cell of the cell at that index.
subCellOfTorus :: (Int, Int) -> Direction -> Lens' (Torus (Cell a)) a
subCellOfTorus pos vn = read2d pos . subcell vn

-- given thesame info as above, toggle the subcell from red to black or back again.
toggleSubCellOnTorus :: (Int, Int) -> Direction -> Torus (Cell RedBlack) -> Torus (Cell RedBlack)
toggleSubCellOnTorus pos vn = over (subCellOfTorus pos vn) toggle

-- A greaterCell is a cell and its sourounding sub-cells
greaterCellFromTorus :: (Int, Int) -> Lens' (Torus (Cell RedBlack)) (Greater cell RedBlack)
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
pairLenses :: forall t a b. ALens' t a -> ALens' t b -> Lens' t (a, b)
pairLenses lens1 lens2 = lens get' put'
  where
    get' t = (t ^# lens1, t ^# lens2)
    put' t (v1, v2) = storing lens2 v2 (storing lens1 v1 t)

-- Turn a cell of lenses into a lense to a cell.
-- this is named after sequenceM's behavior on lists
sequenceL :: forall t a. Cell (ALens' t a) -> Lens' t (Cell a)
sequenceL (Cell (yz, xz, xy, zy, zx, yx)) = split' . cellify
  where
    split' :: Lens' t (a,(a,(a,(a,(a,a)))))
    split' = 
      pairLenses yz . pairLenses xz . pairLenses xy . pairLenses zy . pairLenses zx . pairLenses yx
    cellify :: Iso' (a,(a,(a,(a,(a,a))))) (Cell a)
    cellify = iso f g
    f (yz',(xz',(xy',(zy',(zx',yx'))))) = cell yz' xz' xy' zy' zx' yx'
    g (Cell (yz', xz', xy', zy', zx', yx')) = (yz',(xz',(xy',(zy',(zx',yx')))))

data TorusEx = TorusEx {_torus :: Torus (Cell RedBlack), _headSet :: Set (Int, Int)}

makeLenses ''TorusEx

asLHS, asRHS :: Prism' (Greater cell RedBlack) (Cell (Maybe RedBlack)) -- "Nothing" means a head
asLHS = prism' _put _test
  where
    _put c =
      let f def = (\ dir -> fromMaybe def (c ^. subcell dir) ) 
      in (f Red, f Black) ^. from greaterCell
    _test old= executingStateT (cell Nothing Nothing Nothing Nothing Nothing Nothing) $
        allDirections `Vector.forM_` 
          \ dir -> 
            do
              let f io = old ^. greaterToSubcell io dir
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

eversion :: Iso' (Greater cell a) (Greater cell a)
eversion = from greaterCell . swap . greaterCell
  where
    swap (i,o) = (o,i)

word8 :: Lens' Word8 (Cell RedBlack)
word8 = lens _get _put
  where
    toRB b = if b then Black else Red
    bitToRB x n = toRB $ x `testBit` n 
    _get w = let [a,b,c,d,e,f] = bitToRB w <$> [0..5] in cell a b c d e f
    fromRB rb i = if rb == Black then bit i else 0
    _put w (Cell (a, b, c, d, e, f)) = 
      Vector.foldr (.|.) 0 $ w .&. (bit 7 .|. bit 6) : Vector.zipWith fromRB [a,b,c,d,e,f] [0..5]

word16 :: Iso' Word16 (Greater cell RedBlack)
word16 = from greaterCell . alongside word8 word8 . pack
  where
    pack :: Iso' (Word8, Word8) Word16
    pack = iso h g
    h :: (Word8, Word8) -> Word16
    h (i, o) = shiftL 6 (fromIntegral i) .|. o
    g w = (fromIntegral (shiftR 6 w), fromIntegral (w .&. Vector.foldr (.|.) (bit <$> [0..5]))) 

-- we only actually use the botom 12 bits
ruleVector :: Vector (Greater cell RedBlack)
ruleVector = generate (2^12) (codeToMapEntry . fromIntegral) 

codeToMapEntry :: Word16 -> Maybe (Greater cell RedBlack)
codeToMapEntry lhsCode =
  do
    lhs <- old ^? asLHS
    let heads = Set.filter (^. isHead) allDirections
    guard (not $ Vector.null heads) -- throwout ones that have no heads
    pure (rhs heads ^. from asRHS)
  where
    old :: Greater cell RedBlack
    old = lhsCode ^. from word16
    toBody' :: Direction -> Body (Maybe RedBlack)
    toBody' dir = (old ^. inside) . toBody
    lookupBody map headDir = let (_, body) = toBody' headDir in Map.lookup body map
    newHeads = Set.map (\ headDir -> fromJust look moveMap)
    newBlack heads = fromMaybe (old ^. inside) . ala First Vector.sum $ lookupBody toggleMap <$> heads
    rhs heads = (\dir -> if dir `Set.member` newHeads heads then Nothing else Just (if dir `Set.member` newBlack then Black else Red)) ^. toCell

isHead :: Direction -> Lens' (Cell (Maybe RedBlack)) Bool
isHead dir = subcell dir . to isNothing

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
        (old ^. torus . lookupGreaterCell pos :: Maybe (Greater cell RedBlack, Direction))

applyRuleResult ::
  (Int, Int) ->
  (Greater cell RedBlack, Direction) ->
  (Torus (Cell RedBlack) -> Torus (Cell RedBlack), Set (Int, Int) -> Set (Int, Int))
applyRuleResult pos = applyRuleToTorus pos *** applyRuleToHeadSet pos

lookupGreaterCell :: Getter (Greater cell RedBlack) (Maybe (Greater cell RedBlack))
lookupGreaterCell = set word16 . to (ruleVector !)

lookupPos :: (Int, Int) -> Getter (Torus (Cell RedBlack)) (Greater cell RedBlack)
lookupPos pos = greaterCellFromTorus pos . lookupGreaterCell

isLhzHead :: (Int, Int) -> Getter (Torus (Cell RedBlack)) Bool
isLhzHead pos = lookupPos pos . to isJust

applyRuleToTorus :: (Int, Int) -> Greater cell RedBlack -> Torus (Cell RedBlack) -> Torus (Cell RedBlack)
applyRuleToTorus pos newGC = greaterCellFromTorus pos .~ newGC

applyRuleToHeadSet :: (Int, Int) -> Direction -> Set (Int, Int) -> Set (Int, Int)
applyRuleToHeadSet pos newHead = Set.insert (pos `offset` newHead) . Set.delete pos

mkTorusEx :: Torus (Cell RedBlack) -> TorusEx
mkTorusEx t =
  TorusEx
    { _torus = t,
      _headSet =
        Set.fromAscList . Vector.toList . Vector.filter (\ p -> t ^. isLhzHead p) $ rangeT t
    } 