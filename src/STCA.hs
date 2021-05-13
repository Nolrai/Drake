{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}

module STCA
  (
    VN(..),
    offset,
    Cell(),
    cell,
    readCell,
  )
  where

import Drake

data VN = N | E | S | W
  deriving (Show, Read, Eq, Ord, Enum)

offset :: (Num a) => (a, a) -> VN -> (a, a)
offset (x, y) N = (x, y - 1)
offset (x, y) E = (x + 1, y)
offset (x, y) S = (x, y + 1)
offset (x, y) W = (x - 1, y)

data Cell a = Cell {-# UNPACK #-} (a, a, a, a)

cell n e s w = Cell (n,e,s,w)

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
