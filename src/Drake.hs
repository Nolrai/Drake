{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    Torus (..),
    read2d,
    rangeT,
    rangeMod,
    rangeDivMod,
    torusSize,
  )
where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Lens (Lens', lens)
import Data.Array (range)
import Data.Vector as V
  ( Vector,
    generate,
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
    and,
    div,
    mod,
    (&&),
    (<),
    (<$>),
  )

projectName :: String
projectName = "Drake"

rangeMod :: Int -> Int -> Int
a `rangeMod` b = if x * b < 0 then x + b else x
  where
    x = a `mod` b

rangeDivMod_ :: Int -> Int -> (Int, Int)
a `rangeDivMod_` b = (a `div` b, a `rangeMod` b)

unRangeDivMod_ :: (Int, Int) -> Int -> Int
(d, m) `unRangeDivMod_` y = d * y + m

rangeDivMod :: Int -> Iso' Int (Int, Int)
rangeDivMod b = iso (`rangeDivMod_` b) (`unRangeDivMod_` b)

data Torus a = Torus {widthT :: Int, vectorT :: V.Vector a}
  deriving stock (Show, Read)

rangeT :: Torus a -> Vector (Int, Int)
rangeT tz = generate (V.length (vectorT tz)) (^. rangeDivMod (widthT tz))

instance Functor Torus where
  fmap f t@Torus {vectorT} = t {vectorT = fmap f vectorT}

read2dAux :: Torus a -> (Int, Int) -> Int
Torus {..} `read2dAux` (i, j) = (i `rangeMod` widthT) + (j `rangeMod` (V.length vectorT `div` widthT)) * widthT

read2d_ :: Torus a -> (Int, Int) -> a
tz `read2d_` p = vectorT tz ! (tz `read2dAux` p)

write2d_ :: Torus a -> (Int, Int) -> a -> Torus a
write2d_ t@Torus {..} pos value = t {vectorT = V.modify (\v -> MV.write v (t `read2dAux` pos) value) vectorT}

read2d :: (Int, Int) -> Lens' (Torus a) a
read2d pos = lens get put
  where
    get :: Torus a -> a
    get = (`read2d_` pos)
    put :: Torus a -> a -> Torus a
    put = (`write2d_` pos)

torusSize :: Torus a -> (Int, Int)
torusSize Torus {widthT, vectorT} = (widthT, V.length vectorT `div` widthT)

instance Eq a => Eq (Torus a) where
  l == r = torusSize l == torusSize r && and ((\n -> l ^. read2d n == r ^. read2d n) <$> range ((0, 0), end))
    where
      end = (w -1, h -1)
      (w, h) = torusSize l
