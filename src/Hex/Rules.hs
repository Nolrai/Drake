{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hex.Rules
  ( moveRules,
    toggleRules,
    rotateBy,
    vnDiff,
    readBody,
    toBody,
    toHead,
    RelativeDirection (..),
    LhsTemplate (),
    mkLHS,
    RedBlack (..),
    toggle,
    Body (..),
  )
where

import Control.Lens
import Data.Map as Map (Map, fromList, lookup)
import Relude
  ( Applicative (pure),
    Eq,
    Generic,
    Map,
    Ord,
    Read,
    Show,
    ($),
    (.),
    (<>),
    id,
    executingState,
    flip,
    (/=)
  )
import Data.Maybe
import Hex.Direction (Direction (..), inv, rotateClockwise, allDirections)
import Hex.Cell
import Data.Array

data RelativeDirection
  = SharpLeft
  | DiagonalLeft
  | Across
  | DiagonalRight
  | SharpRight
  deriving stock (Eq, Ord, Show, Read, Generic)

allRelativeDirections :: [RelativeDirection]
allRelativeDirections = 
  [ SharpLeft
  , DiagonalLeft
  , Across
  , DiagonalRight
  , SharpRight
  ]

data RedBlack
  = Red -- Empty
  | Black -- Full
  deriving stock (Eq, Ord, Show, Read, Generic)

toggle :: RedBlack -> RedBlack
toggle Red = Black
toggle Black = Red

{-# ANN module "HLint: use newtype instead of data" #-}

data Body a = Body
  { _atSharpLeft :: a
  , _atDiagonalLeft :: a 
  , _atAcross :: a
  , _atDiagonalRight :: a
  , _atSharpRight :: a
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

makeLenses ''Body

rotateBy :: Direction -> Maybe RelativeDirection -> Direction 
rotateBy = flip rotateBy'
  where
    rotateBy' Nothing = id
    rotateBy' (Just a) = rotateBy'' a
    rotateBy'' SharpLeft = rotateClockwise
    rotateBy'' DiagonalLeft = rotateClockwise . rotateClockwise
    rotateBy'' Across = rotateClockwise . rotateBy'' DiagonalLeft
    rotateBy'' DiagonalRight = rotateClockwise  . rotateBy'' Across
    rotateBy'' SharpRight = rotateClockwise  . rotateBy'' DiagonalRight

dirDiff :: Direction -> Direction -> Maybe RelativeDirection
a `dirDiff` b = lookup (a, b) dirDiffMap

dirDiffMap :: Map.Map (Direction, Direction) RelativeDirection
dirDiffMap = Map.fromList dirDiffList

dirDiffList =
  do
    a <- allDirections
    b <- allRelativeDirections
    pure ((a, a `rotateBy` Just b), b)

readBody :: RelativeDirection -> Lens' (Body a) a
readBody SharpLeft = atSharpLeft
readBody DiagonalLeft = atDiagonalLeft
readBody Across = atAcross
readBody DiagonalRight = atDiagonalRight
readBody SharpRight = atSharpRight

toBody :: Iso' (RelativeDirection -> a) (Body a)
toBody = iso h g
  where
  h f = 
    Body 
      (f SharpLeft)
      (f DiagonalLeft)
      (f Across)
      (f DiagonalRight)
      (f SharpRight)
  g body dir = body ^. readBody dir

onRight :: (AnIso' a b) -> Iso' (c, a) (c, b)
onRight i = iso h g
  where
    h (c, a) = (c, a ^. cloneIso i)
    g (c, b) = (c, b ^. from (cloneIso i))

cellToBody :: forall a. Direction -> Iso' (Cell a) (a, Body a)
cellToBody dir = from toCell . iso h g . onRight toBody
  where
    h :: (Direction -> a) -> (a, RelativeDirection -> a)
    h f = (f dir, \rdir -> f (dir `rotateBy` Just rdir))
    g :: (a, RelativeDirection -> a) -> Direction -> a
    g (a, f) dir' = maybe a f (dir `dirDiff` dir')
        
toggleOpenedGate = Body Red Black Red Black Red  
toggleClosedGate = Body Red Black Black Red Red

queryGateOpenRight = Body Black Red Black Red Red
queryGateOpenLeft = Body Red Red Black Red Black

queryGateClosedRight = Body Black Black Red Red Red
queryGateClosedLeft = Body Red Red Black Black Red

wall1 = Body Black Black Black Red Red
wall2 = Body Red Red Black Black Black 
wall3 = Body Red Black Black Black Red

toggleRules = [(a,b),(b,a)]
  where
    (a,b) = (toggleOpenedGate, toggleClosedGate)

queryRules = 
    [ (queryGateOpenRight, Just DiagonalRight)
    , (queryGateOpenLeft, Just DiagonalLeft)
    , (queryGateClosedRight, Nothing)
    , (queryGateClosedLeft, Nothing)
    ]

wallRules =
  [ (wall1, DiagonalRight)
  , (wall2, DiagonalLeft)
  , (wall3, SharpLeft)
  ]

moveRule = (allRed, Across)