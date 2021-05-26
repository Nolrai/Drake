{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Draw
  ( Draw (..),
  )
where

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
  )
import Relude
  ( Applicative (pure),
    Bool,
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
    allVonNeuman,
    readCell,
    readTemplateFromTorus,
  )

class Draw a b | a -> b where
  draw :: b -> a -> Picture

aroundZero :: Int -> [Int]
aroundZero n = [- (n `div` 2) .. (n `div` 2) + (if even n then -1 else 0)]

instance Draw (TorusZipper (Cell RedBlack)) (Set (GreaterCell RedBlack), (Int, Int), Float) where
  draw (rules, (width, hight), tileSize) tz =
    pictures $ do
      idx <- aroundZero width
      idy <- aroundZero hight
      pure $
        translate
          (fromIntegral idx * tileSize)
          (fromIntegral idy * tileSize)
          ( draw
              tileSize
              ( tz `read2d` (idx, idy),
                (tz `readTemplateFromTorus` (idx, idy)) `member` rules
              )
          )

instance Draw (Cell RedBlack) Float where
  draw tileSize c = pictures $ zipWith applyColor allVonNeuman (triangles tileSize)
    where
      applyColor :: VonNeumann -> Picture -> Picture
      applyColor vn = color (if readCell c vn == Black then light blue else dark orange)

instance Draw (Cell RedBlack, Bool) Float where
  draw tileSize (c, b) = pictures $ zipWith applyColor allVonNeuman (triangles tileSize)
    where
      applyColor :: VonNeumann -> Picture -> Picture
      applyColor vn = color $ (if b then dark else light) (if c `readCell` vn == Black then cyan else orange)

triangles :: Float -> [Picture]
triangles tileSize = zipWith rotate [0, 90, 180, -90] (replicate 4 (triangleNorth tileSize))

triangleNorth :: Float -> Picture
triangleNorth tileSize = polygon [(- tileSize / 2, - tileSize / 2), (tileSize / 2, - tileSize / 2), (0, 0)]
