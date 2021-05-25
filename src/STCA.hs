{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE RecordWildCards #-}
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
    Template (),
    template,
    readTemplate,
    readTemplateFromTorus,
    lhzMap,
    findHeadCells,
  )
where

-- import Data.Set as Set
import Data.Map as M ( fromList, lookup, Map )
import Drake ( TorusZipper, rangeT, read2d, write2d )
import Control.Monad (guard)
-- import Data.Vector as V
import Relude

-- the vonNeumann neihboorhood
data VonNeumann = N | E | S | W
  deriving stock (Show, Read, Eq, Ord, Enum)

allVN :: [VonNeumann]
allVN = [N, E, S, W]

inv :: VonNeumann -> VonNeumann
inv N = S
inv E = W
inv S = N
inv W = E

offset :: (Num a) => (a, a) -> VonNeumann -> (a, a)
offset (x, y) N = (x, y - 1)
offset (x, y) E = (x + 1, y)
offset (x, y) S = (x, y + 1)
offset (x, y) W = (x - 1, y)

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data Cell a = Cell {-# UNPACK #-} !(a, a, a, a)
  deriving stock (Ord, Eq, Read, Functor, Show)

cell :: a -> a -> a -> a -> Cell a
cell n e s w = Cell (n, e, s, w)

writeCell :: Cell a -> VonNeumann -> a -> Cell a
writeCell (Cell (n, e, s, w)) nv v =
  case nv of
    N -> Cell (v, e, s, w)
    E -> Cell (n, v, s, w)
    S -> Cell (n, e, v, w)
    W -> Cell (n, e, s, v)

readCell :: Cell a -> VonNeumann -> a
readCell (Cell (n, _, _, _)) N = n
readCell (Cell (_, e, _, _)) E = e
readCell (Cell (_, _, s, _)) S = s
readCell (Cell (_, _, _, w)) W = w

writeCellOnTorus :: (Int, Int) -> VonNeumann -> a -> TorusZipper (Cell a) -> TorusZipper (Cell a)
writeCellOnTorus pos vn value tz = write2d tz pos (writeCell (tz `read2d` pos) vn value)

toggleCellOnTorus :: (Int, Int) -> VonNeumann -> TorusZipper (Cell WB) -> TorusZipper (Cell WB)
toggleCellOnTorus pos vn tz =
  let targetCell = tz `read2d` pos in
  write2d tz pos (writeCell targetCell vn (not $ targetCell `readCell` vn))

data Template a = Template {-# UNPACK #-} !(Cell a, Cell a)
  deriving stock (Ord, Eq, Read, Functor)

-- A template is a cell and its sourounding sub-cells
template :: Cell a -> Cell a -> Template a
template i o = Template (i, o)

data InsideOutside = Inside | Outside

readTemplate :: Template a -> InsideOutside -> VonNeumann -> a
readTemplate (Template (i, _)) Inside vn = readCell i vn
readTemplate (Template (_, o)) Outside vn = readCell o vn

readTemplateFromTorus :: forall a. TorusZipper (Cell a) -> (Int, Int) -> Template a
readTemplateFromTorus tz pos =
  Template (tz `read2d` pos, readOffset <$> cell N E S W)
  where
    readOffset :: VonNeumann -> a
    readOffset vn = (tz `read2d` offset pos vn) `readCell` inv vn

data LAR = 
  L | -- Left 
  A | -- Across
  R   -- Right
  deriving stock (Eq, Ord, Show, Read)

data WB =
  Z | -- Empty
  F   -- Full
  deriving stock (Eq, Ord, Show, Read)

data LhsTemplate = LHS {lhsHead :: VonNeumann, lhsBody :: Body WB}
data Body a = Body {atL :: a, atA :: a, atR :: a}
data RhsTemplate = RHS {rhs_head :: LAR, rhs_body :: Body WB}

lhs `readLhs` lar = lhsBody lhs `readBody` lar

readBody :: Body a -> LAR -> a
body `readBody` L = atL body
body `readBody` A = atA body
body `readBody` R = atR body

findHeadCells :: TorusZipper (Cell WB) -> [((Int, Int), VonNeumann)]
findHeadCells tz = 
  do
    pos <- rangeT tz
    let t = readTemplate (tz `readTemplateFromTorus` pos)
    vn <- [N, E, S, W]
    guard (not (t Inside vn) && t Outside vn)
    pure (pos, vn)
    
lhzBase :: [(Body WB, RhsTemplate)]
lhzBase =
  [ (Body Z Z Z, RHS A (Body Z Z Z)) -- move forward
  , (Body Z F Z, RHS R (Body Z Z F)) -- turn Right
  , (Body Z Z F, RHS L (Body Z F Z)) -- turn Left (aka co-Turn Right) 
  , (Body F F Z, RHS A (Body F F Z)) -- toggle memory
  ]

toCell :: (VonNeumann -> a) -> Cell a
toCell f = cell (f N) (f E) (f S) (f W)

rotateClockwise :: VonNeumann -> VonNeumann
rotateClockwise N = E 
rotateClockwise E = S 
rotateClockwise S = W 
rotateClockwise W = N 

rotateLar :: LAR -> VonNeumann -> VonNeumann
rotateLar L = rotateClockwise 
rotateLar A = inv 
rotateLar R = rotateClockwise . rotateClockwise . rotateClockwise

-- Find the LAR that gets your from src to target ('Nothing' means tgt = src)
vnDiff :: VonNeumann -> VonNeumann -> Maybe LAR
vnDiff src tgt = M.lookup (src, tgt) vDiffMap

vDiffMap :: Map (VonNeumann, VonNeumann) LAR
vDiffMap = M.fromList $
  do
    src <- [N, E, S, W]
    lar <- [L, A, R]
    pure ((src, lar `rotateLar` src), lar)

lhsToTemplate :: LhsTemplate -> Template WB
lhsToTemplate LHS {..} =
  template (toCell inside) (toCell outside)
    where
      -- the inside is filled where the template is F
      inside vn = fromMaybe Z ((lhsBody `readBody`) <$> (lhsHead `vnDiff` vn))
      outside vn = if (lhsHead == vn) then F else inside vn -- the outside is also filled in at the head

rhsToTemplate :: VonNeumann -> RhsTemplate -> Template WB
rhsToTemplate old_head RHS{..} = 
  template (toCell inside) (toCell outside)
    where
      new_head :: VonNeumann
      new_head = rhs_head `rotateLar` old_head
      -- the outside is filled only where the template is F
      outside, inside :: VonNeumann -> WB
      outside vn = if ((rhs_body `readBody`) <$> (new_head `vnDiff` vn)) == Just F then F else WB
      -- the inside also filled in at the head the head
      inside vn = if (new_head == vn) then F else outsize vn

lhzMap :: Map (Template WB) (Template WB, LAR)
lhzMap = M.fromList
  (do
    vn <- allVN
    (l, r) <- lhzBase
    pure (lhsToTemplate (LHS vn l), (rhsToTemplate vn r, rhs_head r))
  )

