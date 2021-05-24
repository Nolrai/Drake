{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE RecordWildCards #-}

module STCA
  ( VN (..),
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
    lhz_map,
    find_head_cells,
  )
where

-- import Data.Set as Set
import Data.Map as M
import Drake
import Data.Vector as V

data VN = N | E | S | W
  deriving stock (Show, Read, Eq, Ord, Enum)

all_vn :: [VN]
all_vn = [N, E, S, W]

inv :: VN -> VN
inv N = S
inv E = W
inv S = N
inv W = E

offset :: (Num a) => (a, a) -> VN -> (a, a)
offset (x, y) N = (x, y - 1)
offset (x, y) E = (x + 1, y)
offset (x, y) S = (x, y + 1)
offset (x, y) W = (x - 1, y)

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data Cell a = Cell {-# UNPACK #-} !(a, a, a, a)
  deriving stock (Ord, Eq, Read, Functor, Show)

cell :: a -> a -> a -> a -> Cell a
cell n e s w = Cell (n, e, s, w)

writeCell :: Cell a -> VN -> a -> Cell a
writeCell (Cell (n, e, s, w)) nv v =
  case nv of
    N -> Cell (v, e, s, w)
    E -> Cell (n, v, s, w)
    S -> Cell (n, e, v, w)
    W -> Cell (n, e, s, v)

readCell :: Cell a -> VN -> a
readCell (Cell (n, _, _, _)) N = n
readCell (Cell (_, e, _, _)) E = e
readCell (Cell (_, _, s, _)) S = s
readCell (Cell (_, _, _, w)) W = w

writeCellOnTorus :: (Int, Int) -> VN -> a -> TorusZipper (Cell a) -> TorusZipper (Cell a)
writeCellOnTorus pos vn value tz = write2d tz pos (writeCell (tz `read2d` pos) vn value)

toggleCellOnTorus :: (Int, Int) -> VN -> TorusZipper (Cell Bool) -> TorusZipper (Cell Bool)
toggleCellOnTorus pos vn tz =
  let targetCell = tz `read2d` pos in
  write2d tz pos (writeCell targetCell vn (not $ targetCell `readCell` vn))

data Template a = Template {-# UNPACK #-} !(Cell a, Cell a)
  deriving stock (Ord, Eq, Read, Functor)

template :: Cell a -> Cell a -> Template a
template i o = Template (i, o)

data InsideOutside = Inside | Outside

readTemplate :: Template a -> InsideOutside -> VN -> a
readTemplate (Template (i, _)) Inside vn = readCell i vn
readTemplate (Template (_, o)) Outside vn = readCell o vn

readTemplateFromTorus :: forall a. TorusZipper (Cell a) -> (Int, Int) -> Template a
readTemplateFromTorus tz pos =
  Template (tz `read2d` pos, readOffset <$> cell N E S W)
  where
    readOffset :: VN -> a
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

data LHS_Template = LHS {lhs_head :: VN, lhs_body :: Body WB}
data Body a = Body {atL :: a, atA :: a, atR :: a}
data RHS_Template = RHS {rhs_head :: LAR, rhs_body :: Body WB}

lhs `readLhs` lar = (lhs_body lhs) `readBody` lar

readBody :: Body a -> LAR -> a
body `readBody` L = atL body
body `readBody` A = atA body
body `readBody` R = atR body

findHeadCells :: TorusZipper (Cell Bool) -> [((Int, Int), VN)]
findHeadCells tz = 
  do
    pos <- rangeT tz
    let t = readTemplate (tz `readTemplateFromTorus` pos)
    vn <- [N, E, S, W]
    guard (t Inside vn == False && t Outside vn == True)
    pure (pos, vn)
    
lhz_base :: [(Body WB, RHS_Template)]
lhz_base =
  [ (Body Z Z Z, RHS A (Body Z Z Z)) -- move forward
  , (Body Z F Z, RHS R (Body Z Z F)) -- turn Right
  , (Body Z Z F, RHS L (Body Z F Z)) -- turn Left (aka co-Turn Right) 
  , (Body F F Z, RHS A (Body F F Z)) -- toggle memory
  ]

toCell :: (VN -> a) -> Cell a
toCell f = cell (f N) (f E) (f S) (f W)

rotate_clockwise :: VN -> VN
rotate_clockwise N = E 
rotate_clockwise E = S 
rotate_clockwise S = W 
rotate_clockwise W = N 

rotate_lar :: LAR -> VN -> VN
rotate_lar L = rotate_clockwise 
rotate_lar A = inv 
rotate_lar R = rotate_clockwise . rotate_clockwise . rotate_clockwise

vn_diff :: VN -> VN -> Maybe LAR
vn_diff src tgt = M.lookup (src, tgt) vn_diff_map

vn_diff_map :: Map (VN, VN) LAR
vn_diff_map = M.fromList $
  do
    src <- [N, E, S, W]
    lar <- [L, A, R]
    pure ((src, lar `rotate_lar` src), lar)

lhs_to_template :: LHS_Template -> Template Bool
lhs_to_template LHS {..} =
  template (toCell inside) (toCell outside)
    where
      -- the inside is filled where the template is F
      inside vn = ((lhs_body `readBody`) <$> (lhs_head `vn_diff` vn)) == Just F
      outside vn = (lhs_head == vn) || inside vn -- the outside is also filled in at the head

rhs_to_template :: VN -> RHS_Template -> Template Bool
rhs_to_template old_head RHS{..} = 
  template (toCell inside) (toCell outside)
    where
      new_head :: VN
      new_head = rhs_head `rotate_lar` old_head
      -- the outside is filled only where the template is F
      outside, inside :: VN -> Bool
      outside vn = ((rhs_body `readBody`) <$> (new_head `vn_diff` vn)) == Just F
      -- the inside also filled in at the head the head
      inside vn = (new_head == vn) || outside vn

lhz_map :: Map (Template Bool) (Template Bool, LAR)
lhz_map = M.fromList $
  (do
    vn <- all_vn
    (l, r) <- lhz_base
    pure (lhs_to_template (LHS vn l), (rhs_to_template vn r, rhs_head r))
  )

