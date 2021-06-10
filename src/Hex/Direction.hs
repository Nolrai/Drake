{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hex.Direction (Direction (..), allDirections, inv, offset, rotateClockwise) where

import Relude (Enum, Eq, Generic, Int, Num ((+), (-)), Ord, Read, Show)

-- the vonNeumann neihboorhood
data Direction 
  = YZ -- +Y -Z i.e. N
  | XZ -- +X -Z i.e. NE
  | XY -- and so on, going clockwse
  | ZY
  | ZX
  | YX
  deriving stock (Show, Read, Eq, Ord, Enum, Generic)

allDirections :: [Direction]
allDirections = 
  [YZ
  , XZ
  , XY
  , ZY
  , ZX
  , YX
  ]

inv :: Direction -> Direction
inv YZ = ZY
inv XZ = ZX
inv XY = YX
inv ZY = YZ
inv ZX = XZ
inv YX = XY

offset :: (Int, Int) -> Direction -> (Int, Int)
offset (x, y) YZ = (x  , y+1)
offset (x, y) XZ = (x+1, y  )
offset (x, y) XY = (x+1, y-1)
offset (x, y) ZY = (x  , y-1)
offset (x, y) ZX = (x-1, y  )
offset (x, y) YX = (x-1, y+1)

rotateClockwise :: Direction -> Direction
rotateClockwise YZ = XZ 
rotateClockwise XZ = XY
rotateClockwise XY = ZY
rotateClockwise ZY = ZX
rotateClockwise ZX = YX
rotateClockwise YX = YZ