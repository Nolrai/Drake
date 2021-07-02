{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module DrawableCell (DrawableCell(..), greaterToSubcell) where

import Drake ( Torus )
import GHC.Exts ( IsList(Item), Float, Int )
import Graphics.Gloss
    ( Picture(Polygon), rotate, scale, translate, Path ) 
import Control.Lens ( Lens' )
import Data.Function ((.), ($))
import Relude ( Num (..), (/))
import Greater ( outside, inside, Greater, InsideOutside(..) )
import qualified Hex
import Hex.Direction ( Direction(..) )
import qualified Square
import Square.Direction ( Direction(..) )
import RedBlack

class DrawableCell cell dir rdir | cell -> dir, dir -> rdir, rdir -> cell where
  greaterCellFromTorus :: (Int, Int) -> Lens' (Torus (cell RedBlack)) (Greater cell RedBlack)
  rotatedTriangle :: Float -> dir -> Picture
  allDirections :: (IsList a, GHC.Exts.Item a ~ dir) => a
  subcell :: dir -> Lens' (cell a) a 

greaterToSubcell :: DrawableCell cell dir rdir => InsideOutside -> dir -> Lens' (Greater cell a) a
greaterToSubcell Inside vn = inside . subcell vn
greaterToSubcell Outside vn = outside . subcell vn


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
  rotatedTriangle tileSize dir = 
    rotate angle (triangleYZ tileSize)
    where
      angle = 
        case dir of
        YZ -> 0
        XZ -> 60
        XY -> 120
        ZY -> 180 -- == -180
        ZX -> -120
        YX -> -60

triangleYZ :: Float -> Picture
triangleYZ tileSize = 
  translate 0 (tileSize * shiftFactor)
  . scale (1 - lineThickness) (1 - lineThickness) -- create the border by shrinking it slightly, we have to center it first, and then translate it back
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