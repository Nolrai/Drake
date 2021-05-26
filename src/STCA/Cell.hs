{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.Cell (Cell, cell, readCell, toCell, writeCell) where

import Relude (Eq, Functor, Ord, Read, Show)
import STCA.VonNeumann (VonNeumann (..))

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

toCell :: (VonNeumann -> a) -> Cell a
toCell f = cell (f N) (f E) (f S) (f W)
