{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Draw
  ( Draw (..),
  )
where

import Relude
import Data.Set
import Drake
import Graphics.Gloss
import STCA

class Draw a where
  data DrawInfo a :: Type
  draw :: DrawInfo a -> a -> Picture

aroundZero :: Int -> [Int]
aroundZero n = [- (n `div` 2) .. (n `div` 2) + (if even n then -1 else 0)]

instance Draw (TorusZipper (Cell WB)) where
  draw (rules, (width, hight), tileSize) tz =
    pictures $ do
      idx <- aroundZero width
      idy <- aroundZero hight
      pure $ translate 
        (fromIntegral idx * tileSize) 
        (fromIntegral idy * tileSize) 
        (draw 
          (tz `read2d` (idx, idy)
          , tileSize
          , (tz `readTemplateFromTorus` (idx, idy)) `member` rules
          )
        )
  data DrawInfo (TorusZipper (Cell WB)) = (Set (Template WB), (Int, Int), Float)

instance Draw (Cell WB) where
  draw tileSize c = pictures $ zipWith applyColor [N .. W] (triangles tileSize)
    where
      applyColor :: VonNeumann -> Picture -> Picture
      applyColor vn = color (if readCell c vn then light blue else dark orange)
  data DrawInfo (Cell WB) = Float

instance Draw (Cell WB, Bool) where
  draw tileSize (c, b) = pictures $ zipWith applyColor [N .. W] (triangles tileSize)
    where
      applyColor :: VonNeumann -> Picture -> Picture
      applyColor vn = color $ (if b then dark else light) (if c `readCell` vn then cyan else orange)
  data DrawInfo (Cell WB, Bool) = Float

triangles :: Float -> [Picture]
triangles tileSize = zipWith rotate [0, 90, 180, -90] (replicate 4 (triangleNorth tileSize))

triangleNorth :: Float -> Picture
triangleNorth tileSize = polygon [(- tileSize / 2, - tileSize / 2), (tileSize / 2, - tileSize / 2), (0, 0)]
