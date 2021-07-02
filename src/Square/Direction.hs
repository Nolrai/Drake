{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}

module Square.Direction (Direction (..), allDirections, inv, offset, rotateClockwise) where

import Relude (Enum, Eq, Generic, Num ((+), (-)), Ord, Read, Show)
import GHC.Exts

-- the vonNeumann neihboorhood
data Direction = N | E | S | W
  deriving stock (Show, Read, Eq, Ord, Enum, Generic)

allDirections :: (IsList a, GHC.Exts.Item a ~ Direction) => a
allDirections = [N, E, S, W]

inv :: Direction -> Direction
inv N = S
inv E = W
inv S = N
inv W = E

offset :: (Int, Int) -> Direction -> (Int, Int)
offset (x, y) N = (x, y + 1)
offset (x, y) E = (x + 1, y)
offset (x, y) S = (x, y - 1)
offset (x, y) W = (x - 1, y)

rotateClockwise :: Direction -> Direction
rotateClockwise N = E
rotateClockwise E = S
rotateClockwise S = W
rotateClockwise W = N
