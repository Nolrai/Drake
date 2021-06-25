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

class DrawableCell cell dir rdir | cell -> dir, dir -> rdir, rdir -> cell where
  greaterCellFromTorus :: (Int, Int) -> Lens' (Torus (cell a)) (Greater cell a)
  rotatedTriangle :: Float -> dir -> Picture
  allDirections :: GHC.Exts.Item a ~ dir => a
  subcell :: dir -> Lens' (cell a) a 

