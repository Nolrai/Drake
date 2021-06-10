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
import Relude
import STCA

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
          (fromIntegral idy * tileSize + tileSize / 2)
          ( draw
              tileSize
              ( tz ^. read2d (idx, idy),
                (tz ^. greaterCellFromTorus (idx, idy)) `member` rules
              )
          )

instance Draw (Cell RedBlack) Float where
  draw tileSize c = draw tileSize (c, False)

instance Draw (Cell RedBlack, Bool) Float where
  draw tileSize (c, b) = 
    pictures $ coloredTriangle tileSize toColor' <$> allDirections
    where
      toColor' vn = toColor b (c ^. subcell vn)

coloredTriangle :: Float -> (Direction -> Color) -> Direction -> Picture
coloredTriangle tileSize cellColor vn = color (cellColor vn) $ rotatedTriangle tileSize vn

toColor :: Bool -> RedBlack -> Color
toColor True Red = dark orange
toColor True Black = blue
toColor False Red = dim (dark red)
toColor False Black = black

rotatedTriangle :: Float -> Direction -> Picture
rotatedTriangle tileSize vn = 
  rotate angle (triangleNorth tileSize)
  where
    angle = 
      case vn of
        N -> 0
        E -> 90
        S -> 180
        W -> 270

triangleNorth :: Float -> Picture
triangleNorth tileSize = 
  translate 0 (tileSize * shiftFactor)
  . scale (1 - lineThickness) (1 - lineThickness)
  . translate 0 (-tileSize * shiftFactor)
  $ Polygon ((0,0) : northBase tileSize)
  where
    offset = 1/16 -- shift it a little bit more so the edge at the top is thinner.
    toCenter = 1/4 -- move the triangle so its centered.
    shiftFactor :: Float
    shiftFactor = toCenter + offset
    lineThickness :: Float
    lineThickness = 1/16

northBase :: Float -> Path
northBase tileSize = 
  [ (- tileSize / 2, tileSize / 2)
  , (tileSize / 2, tileSize / 2)
  ]