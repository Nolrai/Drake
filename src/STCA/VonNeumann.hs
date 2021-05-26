{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.VonNeumann where

import Relude (Enum, Eq, Num ((+), (-)), Ord, Read, Show)

-- the vonNeumann neihboorhood
data VonNeumann = N | E | S | W
  deriving stock (Show, Read, Eq, Ord, Enum)

allVonNeuman :: [VonNeumann]
allVonNeuman = [N, E, S, W]

inv :: VonNeumann -> VonNeumann
inv N = S
inv E = W
inv S = N
inv W = E

offset :: (Num a) => (a, a) -> VonNeumann -> (a, a)
offset (x, y) N = (x, y - 1)
offset (x, y) E = (x + 1, y)
offset (x, y) S = (x, y + 1)
offset (x, y) W = (x - 1, y)

rotateClockwise :: VonNeumann -> VonNeumann
rotateClockwise N = E
rotateClockwise E = S
rotateClockwise S = W
rotateClockwise W = N
