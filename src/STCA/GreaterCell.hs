{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.GreaterCell
  ( GreaterCell (..),
    InsideOutside (Inside, Outside),
    readTemplate,
    template,
  )
where

import Relude (Eq, Functor, Ord, Read)
import STCA.Cell
import STCA.VonNeumann (VonNeumann (..))

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data GreaterCell a = GreaterCell {-# UNPACK #-} !(Cell a, Cell a)
  deriving stock (Ord, Eq, Read, Functor)

-- A template is a cell and its sourounding sub-cells
template :: Cell a -> Cell a -> GreaterCell a
template i o = GreaterCell (i, o)

data InsideOutside = Inside | Outside

readTemplate :: GreaterCell a -> InsideOutside -> VonNeumann -> a
readTemplate (GreaterCell (i, _)) Inside vn = readCell i vn
readTemplate (GreaterCell (_, o)) Outside vn = readCell o vn
