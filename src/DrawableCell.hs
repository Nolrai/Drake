{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module DrawableCell where

import Drake
import GHC.Exts
import Graphics.Gloss
import Control.Lens (Iso', Lens', iso, lens)
import Relude (Eq, Functor, Generic, Ord, Read, Show, (.))

data RedBlack
  = Red -- Empty
  | Black -- Full
  deriving stock (Eq, Ord, Show, Read, Generic)

toggle :: RedBlack -> RedBlack
toggle Red = Black
toggle Black = Red

class DrawableCell cell dir rdir | cell -> dir, dir -> rdir, rdir -> cell where
  greaterCellFromTorus :: (Int, Int) -> Lens' (Torus (cell a)) (Greater cell a)
  rotatedTriangle :: Float -> dir -> Picture
  allDirections :: GHC.Exts.Item a ~ dir => a
  subcell :: dir -> Lens' (cell a) a 

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data Greater cell a = Greater {-# UNPACK #-} !(cell a, cell a)
  deriving stock (Ord, Eq, Functor, Generic, Show, Read)

greaterCell :: Iso' (cell a, cell a) (Greater cell a)
greaterCell = iso (\(i, o) -> Greater (i, o)) (\(Greater (i, o)) -> (i, o))

data InsideOutside = Inside | Outside
  deriving stock (Show, Read, Eq, Ord)

inside :: Lens' (Greater cell a) (cell a)
inside = lens (\(Greater (i, _)) -> i) (\(Greater (_, o)) i -> Greater (i, o))

outside :: Lens' (Greater cell a) (cell a)
outside = lens (\(Greater (_, o)) -> o) (\(Greater (i, _)) o -> Greater (i, o))

greaterToSubcell :: DrawableCell cell dir rdir => InsideOutside -> dir -> Lens' (Greater cell a) a
greaterToSubcell Inside vn = inside . subcell vn
greaterToSubcell Outside vn = outside . subcell vn