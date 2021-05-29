{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Draw
  ( Draw (..),
  )
where

import Control.Lens ((^.))
import Data.Set
import Drake
import Graphics.Gloss
  ( Picture,
    blue,
    color,
    cyan,
    dark,
    light,
    orange,
    pictures,
    polygon,
    rotate,
    translate,
    red,
    green,
    black,
    Color,
    dim,
  )
import Relude
  ( Applicative (pure),
    Bool(..),
    Eq (..),
    Float,
    Fractional ((/)),
    Int,
    Integral (div),
    Num ((*), (+)),
    even,
    fromIntegral,
    replicate,
    zipWith,
    ($),
  )
import STCA
  ( Cell,
    GreaterCell,
    RedBlack (..),
    VonNeumann (),
    allVonNeumann,
    greaterCellFromTorus,
    subcell,
  )

class Draw a b | a -> b where
  draw :: b -> a -> Picture

aroundZero :: Int -> [Int]
aroundZero n = [- (n `div` 2) .. (n `div` 2) + (if even n then -1 else 0)]

instance Draw (Torus (Cell RedBlack)) (Set (GreaterCell RedBlack), (Int, Int), Float) where
  draw (rules, (width, hight), tileSize) tz =
    pictures $ do
      idx <- aroundZero width
      idy <- aroundZero hight
      pure $
        translate
          (fromIntegral idx * tileSize)
          (fromIntegral idy * tileSize + tileSize/2)
          ( draw
              tileSize
              ( tz ^. read2d (idx, idy),
                (tz ^. greaterCellFromTorus (idx, idy)) `member` rules
              )
          )

instance Draw (Cell RedBlack) Float where
  draw tileSize c = pictures $ zipWith applyColor allVonNeumann (triangles tileSize)
    where
      applyColor :: VonNeumann -> Picture -> Picture
      applyColor vn = color (if c ^. subcell vn == Black then light blue else dark orange)

instance Draw (Cell RedBlack, Bool) Float where
  draw tileSize (c, b) = pictures $ zipWith applyColor allVonNeumann (triangles tileSize)
    where
      applyColor :: VonNeumann -> Picture -> Picture
      applyColor vn = color $ toColor b (c ^. subcell vn)

toColor :: Bool-> RedBlack-> Color
toColor True Red = dark orange
toColor True Black = blue
toColor False Red = dim (dark red)
toColor False Black = black

triangles :: Float -> [Picture]
triangles tileSize = zipWith rotate [0, 90, 180, -90] (replicate 4 (triangleNorth tileSize))

triangleNorth :: Float -> Picture
triangleNorth tileSize = polygon [(- tileSize / 2, tileSize / 2), (tileSize / 2, tileSize / 2), (0, 0)]
