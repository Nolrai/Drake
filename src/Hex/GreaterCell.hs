{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hex.GreaterCell
  ( GreaterCell (),
    InsideOutside (Inside, Outside),
    greaterToSubcell,
    greaterCell,
    inside,
    outside,
  )
where

import Control.Lens (Iso', Lens', iso, lens)
import Relude (Eq, Functor, Generic, Ord, Read, Show, (.))
import Hex.Cell (Cell, subcell)
import Hex.Direction (Direction (..))

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data GreaterCell a = GreaterCell {-# UNPACK #-} !(Cell a, Cell a)
  deriving stock (Ord, Eq, Functor, Generic, Show, Read)

greaterCell :: Iso' (Cell a, Cell a) (GreaterCell a)
greaterCell = iso (\(i, o) -> GreaterCell (i, o)) (\(GreaterCell (i, o)) -> (i, o))

data InsideOutside = Inside | Outside
  deriving stock (Show, Read, Eq, Ord)

inside :: Lens' (GreaterCell a) (Cell a)
inside = lens (\(GreaterCell (i, _)) -> i) (\(GreaterCell (_, o)) i -> GreaterCell (i, o))

outside :: Lens' (GreaterCell a) (Cell a)
outside = lens (\(GreaterCell (_, o)) -> o) (\(GreaterCell (i, _)) o -> GreaterCell (i, o))

greaterToSubcell :: InsideOutside -> Direction -> Lens' (GreaterCell a) a
greaterToSubcell Inside dir = inside . subcell dir
greaterToSubcell Outside dir = outside . subcell dir
