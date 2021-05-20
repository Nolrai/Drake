{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}

module STCA
  ( VN (..),
    offset,
    Cell (),
    cell,
    readCell,
    writeCell,
    writeCellOnTorus,
    toggleCellOnTorus,
    Template (),
    template,
    readTemplate,
    readTemplateFromTorus,
    ruleLHZ,
  )
where

import Data.Set as Set
import Drake

data VN = N | E | S | W
  deriving stock (Show, Read, Eq, Ord, Enum)

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

convert :: (Int, Int, Int, Int, Int, Int, Int, Int) -> Template Bool
convert (i_top, i_bottom, i_left, i_right, o_top, o_bottom, o_left, o_right) =
  -- T, B, L, R order
  ((== 1) :: Int -> Bool) <$> template (cell i_top i_right i_bottom i_left) (cell o_top o_right o_bottom o_left) -- N, E, S, W order

both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)

ruleLHZ :: Set (Template Bool)
ruleLHZ =
  Set.fromList $
    fst . both convert
      <$> [ ((0, 0, 0, 0, 0, 0, 1, 0), (0, 0, 0, 1, 0, 0, 0, 0)), -- move forward
            ((1, 0, 0, 0, 1, 1, 0, 0), (1, 0, 0, 1, 1, 0, 0, 0)), -- Turn Left (head on)
            ((0, 0, 1, 0, 0, 1, 1, 0), (0, 0, 1, 1, 0, 0, 1, 0)), -- Turn Left (right side collison)
            ((1, 0, 0, 1, 1, 1, 0, 1), (1, 0, 1, 1, 1, 0, 0, 1)), -- Turn Right
            ((0, 0, 1, 1, 0, 1, 1, 1), (1, 1, 0, 1, 1, 1, 0, 0)) -- Toggle Memory
          ]

-- findHeads = (\ pos -> readTemplateFromTorus )