{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hex.Rules
  ( lhzBase,
    rotateLar,
    vnDiff,
    readBody,
    toBody,
    toHead,
    RhsTemplate (),
    RelativeDirection (..),
    LhsTemplate (),
    mkLHS,
    mkRHS,
    RedBlack (..),
    toggle,
    Body (..),
  )
where

import Control.Lens
import Data.Map as M (fromList, lookup)
import Relude
  ( Applicative (pure),
    Eq,
    Generic,
    Map,
    Maybe,
    Ord,
    Read,
    Show,
    ($),
    (.),
    take,
    iterate,
  )
import Hex.Direction (Direction (..), inv, rotateClockwise, allDirections)

data RelativeDirection
  = SharpLeft
  | WideLeft
  | Across
  | WideRight
  | SharpRight
  deriving stock (Eq, Ord, Show, Read, Generic)

allRelativeDirections = 
  [ SharpLeft
  , WideLeft
  , Across
  , WideRight
  , SharpRight
  ]

data RedBlack
  = Red -- Empty
  | Black -- Full
  deriving stock (Eq, Ord, Show, Read, Generic)

toggle :: RedBlack -> RedBlack
toggle Red = Black
toggle Black = Red

data Body a = Body 
  { _atSharpLeft :: a
  , _atWideLeft :: a 
  , _atAcross :: a
  , _atWideRight :: a
  , _atSharpRight :: a
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data LhsTemplate = LHS {_lhsHead :: Direction, _lhsBody :: Body RedBlack}
  deriving stock (Eq, Ord, Show, Read, Generic)

mkLHS :: Direction -> Body RedBlack -> LhsTemplate
mkLHS = LHS

data RhsTemplate = RHS {_rhsHead :: RelativeDirection, _rhsBody :: Body RedBlack}
  deriving stock (Eq, Ord, Show, Read, Generic)

mkRHS :: RelativeDirection -> Body RedBlack -> RhsTemplate
mkRHS = RHS

makeLenses ''Body
makeLenses ''LhsTemplate
makeLenses ''RhsTemplate

readBody :: RelativeDirection -> Lens' (Body a) a
readBody SharpLeft = atSharpLeft
readBody WideLeft = atWideLeft
readBody Across = atAcross
readBody WideRight = atWideRight
readBody SharpRight = atSharpRight

type MoveRuleBase = (Body, Maybe RelativeDirection)
moveForward :: MoveRuleBase
moveForward = (allRed, Just Across)
turnLeft :: [MoveRuleBase]
turnLeft = 
  [ (Body Black Red Red Red Red, Just WideLeft)
  , (Body Red Black Red Red Red, Just SharpLeft)
  , (Body Red Red Black Red Black, Just WideLeft)
  ]
turnRight :: [MoveRuleBase]
turnRight = 
  [ (Body Red Red Red Black Red, Just WideRight)
  , (Body Red Red Black Red Red, Just SharpRight)
  , (Body Black Red Black Red Red, Just WideRight)
  ]
mirror :: [MoveRuleBase]
mirror =
  [ (Body Black Black Red Red Red, Nothing)
  , (Body Red Red Black Black Red, Nothing)
  ]

toggleRules = 
  [ (Body Red Black Red Black Red, RHS Nothing (Body Red Black Black Black Red)) -- turn to mirror
  , (Body Red Black Black Red Red, RHS Nothing (Body Red Black Red Black Red)) -- mirror to turn
  ]

moveRules :: [(Body RedBlack, RhsTemplate)]
moveRules =
  moveForward : turnLeft <> turnRight <> mirror

rotateLar :: RelativeDirection -> Direction -> Direction
rotateLar SharpLeft = rotateClockwise
rotateLar WideLeft = rotateClockwise . rotateClockwise
rotateLar Across = inv
rotateLar WideRight = rotateClockwise . inv
rotateLar SharpRight = rotateClockwise . rotateClockwise . inv

-- Find the RelativeDirection that gets you from src to target by rotateLar ('Nothing' means tgt = src)
vnDiff :: Direction -> Direction -> Maybe RelativeDirection
vnDiff src tgt = M.lookup (src, tgt) vDiffMap

vDiffMap :: Map (Direction, Direction) RelativeDirection
vDiffMap = M.fromList $
  do
    src <- allDirections
    rDir <- allRelativeDirections
    pure ((src, rDir `rotateLar` src), rDir)

class HeadBody a h b | a -> b, a -> h where
  toBody :: Lens' a (Body b)
  toHead :: Lens' a h

instance HeadBody LhsTemplate Direction RedBlack where
  toBody = cloneLens lhsBody
  toHead = cloneLens lhsHead

instance HeadBody RhsTemplate RelativeDirection RedBlack where
  toBody = cloneLens rhsBody
  toHead = cloneLens rhsHead
