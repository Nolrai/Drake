{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Copyright: (c) 2021 Chris A. Upshaw
-- SPDX-License-Identifier: MIT
-- Maintainer: Chris A. Upshaw <chrisaupshaw@gmail.com>
--
-- See README for more info
module Drake
  ( projectName,
    RingZipper,
    TorusZipper,
    read,
    read2d,
    mkTorus,
  )
where

import Control.Comonad
import Data.Vector as V
-- import System.Random
import Text.Show as S

projectName :: String
projectName = "Drake"

instance Functor RingZipper where
  fmap f RingZipper {..} = RingZipper {front = front, vector = fmap f vector}

instance Show a => Show (RingZipper a) where
  show r = S.show $ V.generate (V.length $ vector r) (r `read`)

read :: RingZipper a -> Int -> a
RingZipper {..} `read` i = vector ! (i + front `mod` V.length vector)

instance Comonad RingZipper where
  extract RingZipper {..} = V.head vector
  duplicate r@RingZipper {..} =
    RingZipper
      { front = front,
        vector = V.generate s (\i -> r {front = (i + front) `mod` s})
      }
    where
      s = V.length vector

data RingZipper a = RingZipper {front :: Int, vector :: V.Vector a}
  deriving stock (Eq)

data TorusZipper a = TorusZipper {frontT :: (Int, Int), widthT :: Int, vectorT :: V.Vector a}
  deriving stock (Show, Eq)

mkTorus :: Int -> V.Vector a -> TorusZipper a
mkTorus w v = TorusZipper {frontT = (0,0), widthT = w, vectorT = v}

instance Functor TorusZipper where
  fmap f t@TorusZipper {vectorT} = t {vectorT = fmap f vectorT}

read2dAux :: TorusZipper a -> (Int, Int) -> Int
TorusZipper {..} `read2dAux` (i, j) = (((i + fst frontT) `mod` widthT) + ((j + snd frontT) `mod` (V.length vectorT `div` widthT)) * widthT)

read2d :: TorusZipper a -> (Int, Int) -> a
tz `read2d` p = vectorT tz ! (tz `read2dAux` p)

instance Comonad TorusZipper where
  extract TorusZipper {..} = V.head vectorT
  duplicate t@TorusZipper {..} =
    t {vectorT = V.generate s (\i -> t {frontT = f i})}
    where
      f i = (x i, y i)
      x i = (i + fst frontT) `mod` widthT
      y i = ((i `div` widthT) + snd frontT) `mod` (s `div` widthT)
      s = V.length vectorT
