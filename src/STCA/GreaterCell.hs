{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.GreaterCell
  ( GreaterCell (..),
    InsideOutside (Inside, Outside),
    readGreaterCell,
    greaterCell,
    inside,
    outside,
  )
where

import Control.Lens (Iso', Lens', iso, lens, (^.))
import Relude (Eq, Functor, Ord, Read, Show)
import STCA.Cell (Cell, subcell)
import STCA.VonNeumann (VonNeumann (..))

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data GreaterCell a = GreaterCell {-# UNPACK #-} !(Cell a, Cell a)
  deriving stock (Ord, Eq, Functor)

greaterCell :: Iso' (Cell a, Cell a) (GreaterCell a)
greaterCell = iso (\(i, o) -> GreaterCell (i, o)) (\(GreaterCell (i, o)) -> (i, o))

data InsideOutside = Inside | Outside
  deriving stock (Show, Read, Eq, Ord)

inside :: Lens' (GreaterCell a) (Cell a)
inside = lens (\(GreaterCell (i, _)) -> i) (\(GreaterCell (_, o)) i -> GreaterCell (i, o))

outside :: Lens' (GreaterCell a) (Cell a)
outside = lens (\(GreaterCell (_, o)) -> o) (\(GreaterCell (i, _)) o -> GreaterCell (i, o))

readGreaterCell :: GreaterCell a -> InsideOutside -> VonNeumann -> a
readGreaterCell (GreaterCell (i, _)) Inside vn = i ^. subcell vn
readGreaterCell (GreaterCell (_, o)) Outside vn = o ^. subcell vn
