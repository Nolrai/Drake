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
    Cell,
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
    RelativeDirection,
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
import Hex.Cell (Cell (Cell), Cell, subcell, toCell, cell)
import Hex.Rules
import Hex.Direction (Direction (..), allDirections, inv, offset)
import Greater
import Data.Bits

-- given a x,y pair and a Direction access the sub Cell of the Cell at that index.
subCellOfTorus :: (Int, Int) -> Direction -> Lens' (Torus (Cell a)) a
subCellOfTorus pos vn = read2d pos . subcell vn

-- given thesame info as above, toggle the subcell from red to black or back again.
toggleSubCellOnTorus :: (Int, Int) -> Direction -> Torus (Cell RedBlack) -> Torus (Cell RedBlack)
toggleSubCellOnTorus pos vn = over (subCellOfTorus pos vn) toggle

-- A greaterCell is a Cell and its sourounding sub-cells
greaterCellFromTorus :: (Int, Int) -> Lens' (Torus (Cell RedBlack)) (Greater Cell RedBlack)
greaterCellFromTorus pos = pairLenses readInside readOutside . greaterCell
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

-- Turn a Cell of lenses into a lense to a Cell.
-- this is named after sequenceM's behavior on lists
sequenceL :: forall t a. Cell (ALens' t a) -> Lens' t (Cell a)
sequenceL (Cell (yz, xz, xy, zy, zx, yx)) = split' . cellify
  where
    split' :: Lens' t (a,(a,(a,(a,(a,a)))))
    split' = 
      pairLenses yz . pairLenses xz . pairLenses xy . pairLenses zy $ pairLenses zx yx
    cellify :: Iso' (a,(a,(a,(a,(a,a))))) (Cell a)
    cellify = iso f g
    f (yz',(xz',(xy',(zy',(zx',yx'))))) = cell yz' xz' xy' zy' zx' yx'
    g (Cell (yz', xz', xy', zy', zx', yx')) = (yz',(xz',(xy',(zy',(zx',yx')))))

greaterToSubcell :: InsideOutside -> Direction -> Lens' (Greater Cell a) a
greaterToSubcell io dir = cloneLens (insideOutside io) . subcell dir

data TorusEx = TorusEx {_torus :: Torus (Cell RedBlack), _headSet :: Set (Int, Int)}

makeLenses ''TorusEx

asLHS, asRHS :: Prism' (Greater Cell RedBlack) (Cell (Maybe RedBlack)) -- "Nothing" means a head
asLHS = 
  prism' 
    _put 
    _test
  where
    _put :: Cell (Maybe RedBlack) -> Greater Cell RedBlack
    _put c =
      let f def = (\ dir -> fromMaybe def (c ^. subcell dir) ) 
      in (f Red ^. toCell, f Black ^. toCell) ^. greaterCell
    _test :: Greater Cell RedBlack -> Maybe (Cell (Maybe RedBlack))
    _test old = executingStateT (cell Nothing Nothing Nothing Nothing Nothing Nothing) (stateAction old)
    stateAction old =
        allDirections `Vector.forM_` 
          \ dir -> 
            do
              let f io = old ^. greaterToSubcell io dir
              let (i,o) = (f Inside, f Outside)
              (r :: Maybe RedBlack) <- 
                if i == o
                  then pure (Just i)
                  else if o == Black 
                    then pure Nothing
                    else lift Nothing
              subcell dir .= r

asRHS = eversion . asLHS

eversion :: Iso' (Greater Cell a) (Greater Cell a)
eversion = from greaterCell . swap' . greaterCell
  where
  swap' :: Iso' (a, b) (b, a)
  swap' = iso swap swap

-- the cell only uses 6 of the bits
word8 :: Lens' Word8 (Cell RedBlack)
word8 = lens _get _put
  where
    toRB b = if b then Black else Red
    bitToRB x n = toRB $ x `testBit` n
    _get :: Word8 -> Cell RedBlack
    _get w = bitToRB w <$> Cell (0, 1, 2, 3, 4, 5)
    fromRB :: RedBlack -> Int -> Word8
    fromRB rb i = if rb == Black then bit i else 0
    _put :: Word8 -> Cell RedBlack -> Word8
    _put w (Cell (a, b, c, d, e, f)) = 
      Vector.foldr (.|.) 0 $ vHead `Vector.cons` vTail
      where
      vHead :: Word8
      vHead = w .&. (bit 7 .|. bit 6)  
      vTail :: Vector Word8
      vTail = Vector.zipWith fromRB [a,b,c,d,e,f] ([0..5] :: Vector Int)

word16 :: Lens' Word16 (Greater Cell RedBlack)
word16 = from pack . middle . greaterCell 
  where
    middle :: Lens' (Word8, Word8) (Cell RedBlack, Cell RedBlack)
    middle = alongside word8 word8
    pack :: Iso' (Word8, Word8) Word16
    pack = iso h g
    h :: (Word8, Word8) -> Word16
    h (i, o) = shiftL 6 (fromIntegral i) .|. fromIntegral o
    g :: Word16 -> (Word8, Word8)
    g w = (fromIntegral (shiftR w 6), fromIntegral (w .&. Vector.foldr (.|.) 0 (bit <$> [0..5]))) 

-- we only actually use the botom 12 bits
ruleVector :: Vector (Maybe (Greater Cell RedBlack))
ruleVector = maybe (error "RuleVector Failed") generate (2^12) (codeToMapEntry . fromIntegral)

codeToMapEntry :: Word16 -> Maybe (Greater Cell RedBlack)
codeToMapEntry lhsCode =
  do
    lhs <- old ^? asLHS
    let heads = Set.filter (\dir -> lhs ^. isHead dir) allDirections 
    guard (not $ Set.null heads) -- throwout ones that have no heads
    newHeads <- (`lookUp` moveMap) `Set.map` heads
    pure (rhs heads newHeads ^. from asRHS)
  where
    old :: Greater Cell RedBlack
    old = lhsCode ^. word16
    toBody' :: Direction -> (_, Body (Maybe RedBlack))
    toBody' dir = old ^. inside . toBody dir
    lookupBody ruleMap headDir = let (_, body) = toBody' headDir in Map.lookup body ruleMap
    newBlack heads = fromMaybe (old ^. inside) . ala First Vector.sum $ lookupBody toggleMap <$> heads
    rhs heads newHeads = (\dir -> if dir `Set.member` newHeads then Nothing else Just (if dir `Set.member` newBlack heads then Black else Red)) ^. toCell

isHead :: Direction -> Lens' (Cell (Maybe RedBlack)) Bool
isHead dir = subcell dir . to isNothing

wideStep :: TorusEx -> Int -> TorusEx
wideStep oldState index = if Set.null headSet' then error $ fromString "headSet is null" else applyRule oldState pos
  where
    headSet' = oldState ^. headSet
    pos = (Vector.fromList . Set.toList) headSet' ! (index `rangeMod` Set.size headSet') 

applyRule :: TorusEx -> (Int, Int) -> TorusEx
-- pos is the index of the Cell the head is pointing into
applyRule old pos =
  (torus %~ modifyTorus_) . (headSet %~ modifyHeadSet_) $ old
  where
    modifyTorus_ :: Torus (Cell RedBlack) -> Torus (Cell RedBlack)
    modifyHeadSet_ :: Set (Int, Int) -> Set (Int, Int)
    (modifyTorus_, modifyHeadSet_) =
      maybe
        (id, Set.delete pos)
        (applyRuleResult pos)
        (old ^. torus . lookupPos pos :: Maybe (Greater Cell RedBlack))

applyRuleResult ::
  (Int, Int) ->
  Greater Cell RedBlack ->
  (Torus (Cell RedBlack) -> Torus (Cell RedBlack), Set (Int, Int) -> Set (Int, Int))
applyRuleResult pos = (applyRuleToTorus pos newGC, applyRuleToHeadSet pos newGC)

lookupGreaterCell :: Getter (Greater Cell RedBlack) (Maybe (Greater Cell RedBlack))
lookupGreaterCell = to (join . f)
  where
    f :: Greater Cell RedBlack -> Maybe (Maybe (Greater Cell RedBlack))
    f g = ruleVector !? fromIntegral (set word16 g 0)

lookupPos :: (Int, Int) -> Getter (Torus (Cell RedBlack)) (Maybe (Greater Cell RedBlack))
lookupPos pos = greaterCellFromTorus pos . lookupGreaterCell

isLhzHead :: (Int, Int) -> Getter (Torus (Cell RedBlack)) Bool
isLhzHead pos = lookupPos pos . to isJust

applyRuleToTorus :: (Int, Int) -> Greater Cell RedBlack -> Torus (Cell RedBlack) -> Torus (Cell RedBlack)
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