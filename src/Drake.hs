{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

-- {-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: (c) 2021 Chris A. Upshaw
-- SPDX-License-Identifier: MIT
-- Maintainer: Chris A. Upshaw <chrisaupshaw@gmail.com>
--
-- See README for more info
module Drake
  ( projectName,
    RingZipper (..),
    TorusZipper (..),
    ArrayLike (..),
  )
where

import Control.Comonad
-- import Control.Monad
-- import Data.List (intercalate)
-- import Data.Monoid (Any)
import qualified Data.Vector as V
-- import Diagrams.Backend.CmdLine
-- import Diagrams.Backend.SVG
-- import Diagrams.Core.Compile
-- import Diagrams.Core.Types
-- import Diagrams.Prelude hiding (after)
-- import Diagrams.TwoD
-- import Diagrams.TwoD.Layout.Grid
-- import System.Random
import Text.Show as S

class ArrayLike arr where
  type Index arr
  type Elm arr
  (!) :: arr -> Index arr -> Elm arr

projectName :: String
projectName = "Drake"

-- ignore the index to the front, map on the vector.
instance Functor RingZipper where
  fmap f RingZipper {..} = RingZipper {frontR = frontR, vectorR = fmap f vectorR}

-- only consider them different if they are observibly different.
instance Eq a => Eq (RingZipper a) where
  l == r = size == length (vectorR r) && all (\n -> (l ! n) == (r ! n)) indexes
    where
      indexes :: V.Vector Int
      indexes = [0 .. size]
      size :: Int
      size = length (vectorR l)

instance Comonad RingZipper where
  extract = (! 0)
  duplicate r@RingZipper {..} =
    RingZipper
      { frontR = frontR,
        vectorR = V.generate s (\i -> r {frontR = i `mod` s})
      }
    where
      s = V.length vectorR

data RingZipper a = RingZipper {frontR :: Int, vectorR :: V.Vector a}
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data TorusZipper a = TorusZipper {frontT :: (Int, Int), widthT :: Int, vectorT :: V.Vector a}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance Functor TorusZipper where
  fmap f t@TorusZipper {vectorT} = t {vectorT = fmap f vectorT}

instance Comonad TorusZipper where
  extract = (! (0, 0))
  duplicate t@TorusZipper {..} =
    t {vectorT = V.generate s (\i -> t {frontT = f i})}
    where
      f i = (x i, y i)
      x i = i `mod` widthT
      y i = (i `div` widthT) `mod` (s `div` widthT)
      s = V.length vectorT

instance ArrayLike (TorusZipper a) where
  (!) TorusZipper {..} (i, j) =
    vectorT V.! (((i + fst frontT) `mod` widthT) + ((j + snd frontT) `mod` (V.length vectorT `div` widthT)) * widthT)
  type Index (TorusZipper a) = (Int, Int)
  type Elm (TorusZipper a) = a

instance ArrayLike (RingZipper a) where
  (!) RingZipper {..} i = vectorR V.! ((i + frontR) `mod` V.length vectorR)
  type Index (RingZipper a) = Int
  type Elm (RingZipper a) = a
