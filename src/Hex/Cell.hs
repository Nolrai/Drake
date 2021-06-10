{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hex.Cell (Cell (Cell), cell, toCell, subcell) where

import Control.Lens
import Relude (Eq, Functor, Generic, Ord, Read, Show)
import Hex.Direction (Direction (..))

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data Cell a = Cell {-# UNPACK #-} !(a, a, a, a, a, a)
  deriving stock (Ord, Eq, Read, Functor, Show, Generic)

cell :: a -> a -> a -> a -> Cell a
cell n e s w = Cell ()

writeCell_ :: Cell a -> Direction -> a -> Cell a
writeCell_ (Cell (n, e, s, w)) vn v =
  case vn of
    N -> Cell (v, e, s, w)
    E -> Cell (n, v, s, w)
    S -> Cell (n, e, v, w)
    W -> Cell (n, e, s, v)

readCell_ :: Cell a -> Direction -> a
readCell_ (Cell (n, _, _, _)) N = n
readCell_ (Cell (_, e, _, _)) E = e
readCell_ (Cell (_, _, s, _)) S = s
readCell_ (Cell (_, _, _, w)) W = w

subcell :: Direction -> Lens' (Cell a) a
subcell vn = lens (`readCell_` vn) (`writeCell_` vn)

toCell_ :: (Direction -> a) -> Cell a
toCell_ f = cell (f N) (f E) (f S) (f W)

toCell :: Iso' (Direction -> a) (Cell a)
toCell = iso toCell_ (\c vn -> c ^. subcell vn)
