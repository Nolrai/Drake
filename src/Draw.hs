module Draw where

import Drake
import Graphics.Gloss
import STCA

tileSize :: Float
tileSize = 10.0

class Draw a where
  draw :: a -> Picture

aroundZero :: Int -> [Int]
aroundZero n = [-(n `div` 2) .. (n `div` 2) + (if n `mod` 2 == 0 then -1 else 0)]

-- instance Draw a => Draw (TorusZipper a, Int, Int) where
--   draw (tz, width, hight) = 
--     pictures $ do
--       idx <- aroundZero width
--       idy <- aroundZero hight
--       translate (x * tileSize) (y * tileSize) (draw $ tz `read2d` (idx, idy))

-- instance Draw (Cell Bool) where
--   draw c = pictures $ zipWith applyColor [N .. W] triangles
--     where
--       applyColor vn = (\ix -> color (if readCell c vn then light blue else dark orenge))
--       triangles :: [Picture]
--       triangles = zipWith rotate [0, 90, 180, -90] (replicate trianlgeNorth 4)
--       triangleNorth :: Picture
--       triangleNorth = polygon [(-tileSize/2, -tileSize/2), (tileSize/2, -tileSize/2), (0,0)]
    