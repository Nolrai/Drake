{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.Rules
  ( lhzBase,
    rotateLar,
    vnDiff,
    readBody,
    RhsTemplate (..),
    LAR,
    LhsTemplate (..),
    RedBlack (..),
  )
where

import Data.Map as M (fromList, lookup)
import Relude
  ( Applicative (pure),
    Eq,
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
  deriving stock (Eq, Ord, Show, Read)

data RedBlack
  = Red -- Empty
  | Black -- Full
  deriving stock (Eq, Ord, Show, Read)

data LhsTemplate = LHS {lhsHead :: VonNeumann, lhsBody :: Body RedBlack}

data Body a = Body {atL :: a, atA :: a, atR :: a}

data RhsTemplate = RHS {rhs_head :: LAR, rhs_body :: Body RedBlack}

readBody :: Body a -> LAR -> a
body `readBody` L = atL body
body `readBody` A = atA body
body `readBody` R = atR body

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
