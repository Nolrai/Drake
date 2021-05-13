{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Draw where

import Drake
import Graphics.Gloss
import STCA

tileSize :: Float
tileSize = 10.0

class Draw a b | a -> b where
  draw :: a -> b -> Picture

aroundZero :: Int -> [Int]
aroundZero n = [-(n `div` 2) .. (n `div` 2) + (if n `mod` 2 == 0 then -1 else 0)]

instance Draw a () => Draw (TorusZipper a) (Int, Int) where
  draw tz (width, hight) = 
    pictures $ do
      idx <- aroundZero width
      idy <- aroundZero hight
      pure $ translate (fromIntegral idx * tileSize) (fromIntegral idy * tileSize) (draw (tz `read2d` (idx, idy)) ())

instance Draw (Cell Bool) () where
  draw c _ = pictures $ zipWith applyColor [N .. W] triangles
    where
      applyColor :: VN -> Picture -> Picture
      applyColor vn = color (if readCell c vn then light blue else dark orange)
      triangles :: [Picture]
      triangles = zipWith rotate [0, 90, 180, -90] (replicate 4 triangleNorth)
      triangleNorth :: Picture
      triangleNorth = polygon [(-tileSize/2, -tileSize/2), (tileSize/2, -tileSize/2), (0,0)]
    