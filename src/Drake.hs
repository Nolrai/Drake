{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
    read,
    read2d,
    write2d,
    mkTorus,
    rangeT,
    rangeMod,
  )
where

import Control.Comonad (Comonad (duplicate, extract))
-- import System.Random

import Data.Array (range)
import Data.Vector as V
  ( Vector,
    generate,
    head,
    length,
    modify,
    (!),
  )
import qualified Data.Vector.Mutable as MV
import Relude
  ( Eq (..),
    Functor (fmap),
    Int,
    Num ((*), (+), (-)),
    Read,
    Show,
    String,
    mod,
    div,
    and,
    curry,
    fst,
    snd,
    (&&),
    (<$>),
    (<),
  )

projectName :: String
projectName = "Drake"

instance Functor RingZipper where
  fmap f RingZipper {..} = RingZipper {front = front, vector = fmap f vector}

rangeMod :: Int -> Int -> Int
a `rangeMod` b = if x * b < 0 then x + b else x
  where
    x = a `mod` b

read :: RingZipper a -> Int -> a
RingZipper {..} `read` i = vector ! (i + front `rangeMod` V.length vector)

instance Comonad RingZipper where
  extract RingZipper {..} = V.head vector
  duplicate r@RingZipper {..} =
    RingZipper
      { front = front,
        vector = V.generate s (\i -> r {front = (i + front) `rangeMod` s})
      }
    where
      s = V.length vector

data RingZipper a = RingZipper {front :: Int, vector :: V.Vector a}
  deriving stock (Show, Read)

data TorusZipper a = TorusZipper {frontT :: (Int, Int), widthT :: Int, vectorT :: V.Vector a}
  deriving stock (Show, Read)

rangeT :: TorusZipper a -> [(Int, Int)]
rangeT tz = curry range (0, 0) (widthT tz, V.length (vectorT tz) `div` widthT tz)

mkTorus :: Int -> V.Vector a -> TorusZipper a
mkTorus w v = TorusZipper {frontT = (0, 0), widthT = w, vectorT = v}

instance Functor TorusZipper where
  fmap f t@TorusZipper {vectorT} = t {vectorT = fmap f vectorT}

read2dAux :: TorusZipper a -> (Int, Int) -> Int
TorusZipper {..} `read2dAux` (i, j) = ((i + fst frontT) `rangeMod` widthT) + ((j + snd frontT) `rangeMod` (V.length vectorT `div` widthT)) * widthT

read2d :: TorusZipper a -> (Int, Int) -> a
tz `read2d` p = vectorT tz ! (tz `read2dAux` p)

write2d :: TorusZipper a -> (Int, Int) -> a -> TorusZipper a
write2d t@TorusZipper {..} pos value = t {vectorT = V.modify (\v -> MV.write v (t `read2dAux` pos) value) vectorT}

instance Comonad TorusZipper where
  extract TorusZipper {..} = V.head vectorT
  duplicate t@TorusZipper {..} =
    t {vectorT = V.generate s (\i -> t {frontT = f i})}
    where
      f i = (x i, y i)
      x i = (i + fst frontT) `rangeMod` widthT
      y i = ((i `div` widthT) + snd frontT) `rangeMod` (s `div` widthT)
      s = V.length vectorT

ringSize :: RingZipper a -> Int
ringSize RingZipper {vector} = V.length vector

torusSize :: TorusZipper a -> (Int, Int)
torusSize TorusZipper {widthT, vectorT} = (widthT, V.length vectorT `div` widthT)

instance Eq a => Eq (RingZipper a) where
  l == r = ringSize l == ringSize r && and ((\n -> l `read` n == r `read` n) <$> [0 .. ringSize l - 1])

instance Eq a => Eq (TorusZipper a) where
  l == r = torusSize l == torusSize r && and ((\n -> l `read2d` n == r `read2d` n) <$> range ((0, 0), end))
    where
      end = (w -1, h -1)
      (w, h) = torusSize l
