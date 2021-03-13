{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: (c) 2021 Chris A. Upshaw
-- SPDX-License-Identifier: MIT
-- Maintainer: Chris A. Upshaw <chrisaupshaw@gmail.com>
--
-- See README for more info
module Drake
  ( projectName,
  )
where

import Control.Comonad
import Control.Monad
import Data.List (intercalate)
import Data.Monoid (Any)
import Data.Vector as V
import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG
import Diagrams.Core.Compile
import Diagrams.Core.Types
import Diagrams.Prelude hiding (after)
import Diagrams.TwoD
import Diagrams.TwoD.Layout.Grid
import System.Random

projectName :: String
projectName = "Drake"

instance Functor RingZipper where
  fmap f RingZipper {..} = RingZipper {front = front, vector = fmap f vector}

read :: RingZipper a -> Int -> a
RingZipper {..} `read` i = vector ! (i + front `mod` V.length vector)

duplicate' :: RingZipper a -> RingZipper (RingZipper a)
duplicate' r@RingZipper {..} =
  RingZipper
    { front = front,
      vector = V.generate s (\i -> r {front = (i + front) `mod` s})
    }
  where
    s = V.length vector

instance Comonad RingZipper where
  extract RingZipper {..} = V.head vector
  extend f r = fmap f (duplicate' r)

data RingZipper a = RingZipper {front :: Int, vector :: V.Vector a}

data TorusZipper a = TorusZipper {frontT :: (Int, Int), width :: Int, vectorT :: V.Vector a}

instance Functor TorusZipper where
  fmap f t@TorusZipper {vectorT} = t {vectorT = fmap f vectorT}

duplicate'' t@TorusZipper {..} =
  t {vectorT = V.generate s (\i -> t {frontT = f i})}
  where
    f i = (x i, y i)
    x i = (i + fst frontT) `mod` width
    y i = ((i `div` width) + snd frontT) `mod` (s `div` width)
    s = V.length vectorT

read2d :: TorusZipper a -> (Int, Int) -> a
TorusZipper {..} `read2d` (i, j) =
  vectorT ! (((i + fst frontT) `mod` width) + ((j + snd frontT) `mod` (V.length vectorT `div` width)) * width)

instance Comonad TorusZipper where
  extract r = r `read2d` (0, 0)
  extend f t = fmap f (duplicate'' t)
