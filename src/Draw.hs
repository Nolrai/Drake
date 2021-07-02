{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Draw
  ( Draw (..),
  )
where

import Control.Lens ((^.), to)
import Drake
import Graphics.Gloss
import Relude
import DrawableCell
import RedBlack
import Greater

class Draw a b | a -> b where
  draw :: b -> a -> Picture

aroundZero :: Int -> [Int]
aroundZero n = [- (n `div` 2) .. (n `div` 2) + (if even n then -1 else 0)]

instance DrawableCell cell dir rdir => Draw (Torus (cell RedBlack)) (Greater cell RedBlack -> Bool, (Int, Int), Float) where
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
                tz ^. greaterCellFromTorus (idx, idy) . to rules
              )
          )

instance DrawableCell cell dir rdir => Draw (cell RedBlack) Float where
  draw tileSize c = draw tileSize (c, False)

instance DrawableCell cell dir rdir => Draw (cell RedBlack, Bool) Float where
  draw tileSize (c, b) = 
    pictures $ coloredTriangle tileSize toColor' <$> allDirections
    where
      toColor' :: dir -> Color
      toColor' vn = toColor b (c ^. subcell vn)
  
coloredTriangle :: DrawableCell cell dir rdir => Float -> (dir -> Color) -> dir -> Picture
coloredTriangle tileSize cellColor vn = color (cellColor vn) $ rotatedTriangle tileSize vn

toColor :: Bool -> RedBlack -> Color
toColor True Red = dark orange
toColor True Black = blue
toColor False Red = dim (dark red)
toColor False Black = black