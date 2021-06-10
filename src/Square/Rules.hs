{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.Rules
  ( lhzBase,
    rotateLar,
    vnDiff,
    readBody,
    toBody,
    toHead,
    RhsTemplate (),
    LAR (..),
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
  )
import STCA.VonNeumann (VonNeumann (..), inv, rotateClockwise)

data LAR
  = L -- Left
  | A -- Across
  | R -- Right
  deriving stock (Eq, Ord, Show, Read, Generic)

data RedBlack
  = Red -- Empty
  | Black -- Full
  deriving stock (Eq, Ord, Show, Read, Generic)

toggle :: RedBlack -> RedBlack
toggle Red = Black
toggle Black = Red

data Body a = Body {_atL :: a, _atA :: a, _atR :: a}
  deriving stock (Eq, Ord, Show, Read, Generic)

data LhsTemplate = LHS {_lhsHead :: VonNeumann, _lhsBody :: Body RedBlack}
  deriving stock (Eq, Ord, Show, Read, Generic)

mkLHS :: VonNeumann -> Body RedBlack -> LhsTemplate
mkLHS = LHS

data RhsTemplate = RHS {_rhsHead :: LAR, _rhsBody :: Body RedBlack}
  deriving stock (Eq, Ord, Show, Read, Generic)

mkRHS :: LAR -> Body RedBlack -> RhsTemplate
mkRHS = RHS

makeLenses ''Body
makeLenses ''LhsTemplate
makeLenses ''RhsTemplate

readBody :: LAR -> Lens' (Body a) a
readBody L = atL
readBody A = atA
readBody R = atR

lhzBase :: [(Body RedBlack, RhsTemplate)]
lhzBase =
  [ (Body Red Red Red, RHS A (Body Red Red Red)), -- move forward
    (Body Red Black Red, RHS R (Body Red Red Black)), -- turn Right
    (Body Red Red Black, RHS L (Body Red Black Red)), -- turn Left (aka co-Turn Right)
    (Body Black Black Red, RHS A (Body Black Black Red)) -- toggle memory
  ]

rotateLar :: LAR -> VonNeumann -> VonNeumann
rotateLar L = rotateClockwise
rotateLar A = inv
rotateLar R = rotateClockwise . rotateClockwise . rotateClockwise

-- Find the LAR that gets your from src to target ('Nothing' means tgt = src)
vnDiff :: VonNeumann -> VonNeumann -> Maybe LAR
vnDiff src tgt = M.lookup (src, tgt) vDiffMap

vDiffMap :: Map (VonNeumann, VonNeumann) LAR
vDiffMap = M.fromList $
  do
    src <- [N, E, S, W]
    lar <- [L, A, R]
    pure ((src, lar `rotateLar` src), lar)

class HeadBody a h b | a -> b, a -> h where
  toBody :: Lens' a (Body b)
  toHead :: Lens' a h

instance HeadBody LhsTemplate VonNeumann RedBlack where
  toBody = cloneLens lhsBody
  toHead = cloneLens lhsHead

instance HeadBody RhsTemplate LAR RedBlack where
  toBody = cloneLens rhsBody
  toHead = cloneLens rhsHead
