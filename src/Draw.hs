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
import qualified Hex
import qualified Square
import DrawableCell

class Draw a b | a -> b where
  draw :: b -> a -> Picture

aroundZero :: Int -> [Int]
aroundZero n = [- (n `div` 2) .. (n `div` 2) + (if even n then -1 else 0)]

instance DrawableCell cell dir rdir => Draw (Torus (cell RedBlack)) (greatercell RedBlack -> Bool, (Int, Int), Float) where
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
                tz ^. greaterCellFromTorus (idx, idy) . to highlight
              )
          )

instance DrawableCell cell dir rdir => Draw (cell RedBlack) Float where
  draw tileSize c = draw tileSize (c, False)

instance DrawableCell cell dir => Draw (cell RedBlack, Bool) Float where
  draw tileSize (c, b) = 
    pictures $ coloredTriangle tileSize toColor' <$> Square.allDirections
    where
      toColor' vn = toColor b (c ^. Square.subcell vn)
  
coloredTriangle :: Subcells dir => Float -> (dir -> Color) -> dir -> Picture
coloredTriangle tileSize cellColor vn = color (cellColor vn) $ rotatedTriangle tileSize vn

toColor :: Bool -> RedBlack -> Color
toColor True Red = dark orange
toColor True Black = blue
toColor False Red = dim (dark red)
toColor False Black = black

instance DrawableCell Square.Cell Square.Direction Square.RelativeDirection where
  greaterCellFromTorus = Square.greaterCellFromTorus
  allDirections = Square.allDirections
  subcell = Square.subcell  
  rotatedTriangle tileSize vn = 
    rotate angle (triangleNorth tileSize)
    where
      angle = 
        case vn of -- CSPAM This seems backwards, but works. So I presumebly have something backwards somewhere else canceling it out.
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
  [ (- radius, radius)
  , (radius, radius)
  ]
  where
    radius = tileSize/2

instance DrawableCell Hex.Cell Hex.Direction Hex.RelativeDirection where
  greaterCellFromTorus = Hex.greaterCellFromTorus
  allDirections = Hex.allDirections
  subcell = Hex.subcell
  rotatedTriangle triangle dir= 
    rotate angle (triangleNorth tileSize)
    where
      angle = 
        case vn of
        YZ -> 0
        XZ -> 60
        XY -> 120
        ZY -> 180 -- == -180
        ZX -> -120
        YX -> -60

triangleYZ :: Float -> Picture
triangleYZ tileSize = 
  translate 0 (tileSize * shiftFactor)
  . scale (1 - lineThickness) (1 - lineThickness)
  . translate 0 (-tileSize * shiftFactor)
  $ Polygon ((0,0) : yzBase tileSize)
  where
    offset = 1/16 -- shift it a little bit more so the edge at the top is thinner.
    toCenter = 1/3-- move the triangle so its centered.
    shiftFactor :: Float
    shiftFactor = toCenter + offset
    lineThickness :: Float
    lineThickness = 1/16

yzBase :: Float -> Path
yzBase tileSize =
  [ (-radius/2, radius)
  , (radius/2, radius)
  ] 
  where
    radius = tileSize/2