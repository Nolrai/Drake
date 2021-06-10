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

cell :: a -> a -> a -> a -> a -> a -> Cell a
cell yz xz xy zy zx yx = Cell (yz, xz, xy, zy, zx, yx)

writeCell_ :: Cell a -> Direction -> a -> Cell a
writeCell_ (Cell (yz, xz, xy, zy, zx, yx)) dir v =
  case dir of
    YZ -> Cell (v, xz, xy, zy, zx, yx)
    XZ -> Cell (yz, v, xy, zy, zx, yx)
    XY -> Cell (yz, xz, v, zy, zx, yx)
    ZY -> Cell (yz, xz, xy, v, zx, yx)
    ZX -> Cell (yz, xz, xy, zy, v, yx)
    YX -> Cell (yz, xz, xy, zy, zx, v)

readCell_ :: Cell a -> Direction -> a
readCell_ (Cell (yz, _, _, _, _, _)) YZ = yz
readCell_ (Cell (_, xz, _, _, _, _)) XZ = xz
readCell_ (Cell (_, _, xy, _, _, _)) XY = xy
readCell_ (Cell (_, _, _, zy, _, _)) ZY = zy
readCell_ (Cell (_, _, _, _, zx, _)) ZX = zx
readCell_ (Cell (_, _, _, _, _, yx)) YX = yx

subcell :: Direction -> Lens' (Cell a) a
subcell dir = lens (`readCell_` dir) (`writeCell_` dir)

toCell_ :: (Direction -> a) -> Cell a
toCell_ f = cell (f YZ) (f XZ) (f XY) (f ZY) (f ZX) (f YX)

toCell :: Iso' (Direction -> a) (Cell a)
toCell = iso toCell_ (\c dir -> c ^. subcell dir)
