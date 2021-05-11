{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}

module STCA where

import Drake

data VN = N | E | S | W
  deriving (Show, Read, Eq, Ord, Enum)

offset :: (Num a) => (a, a) -> VN -> (a, a)
offset (x, y) N = (x, y -1)
offset (x, y) E = (x + 1, y)
offset (x, y) S = (x, y + 1)
offset (x, y) W = (x -1, y)

newtype Cell a = Cell {-# UNPACK #-} !(a, a, a, a)

readCell :: Cell a -> VN -> a
readCell (Cell (n, _, _, _)) N = n
readCell (Cell (_, e, _, _)) E = e
readCell (Cell (_, _, s, _)) S = s
readCell (Cell (_, _, _, w)) W = w
