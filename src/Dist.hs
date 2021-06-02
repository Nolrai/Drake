{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Dist (distSize, mkUniform, drawFrom, Dist()) where

import Relude
import Control.Applicative as App
import Data.Map as Map
import Data.IntMap as IntMap
import Data.Set as Set
import System.Random.Stateful

newtype Dist a = Dist {distChoose :: IntMap a}
  deriving newtype (Show, Read, Functor)

distSize :: Dist a -> Int
distSize = distSize' . distChoose

distSize' :: IntMap b -> Int
distSize' = maybe 0 fst . IntMap.lookupMax

mkUniform :: forall a. Ord a => Set a -> Dist a
mkUniform l = Dist {..}
  where
  distChoose = distChooseF sizeMap
  sizeMap ::  Map a Int
  sizeMap = Set.foldl' (\ m x -> Map.insertWith (+) x 1 m) mempty l
  distChooseF :: Map a Int -> IntMap a
  distChooseF = Map.foldlWithKey' toSumMap (mempty :: IntMap a) 
  toSumMap :: IntMap b -> b -> Int -> IntMap b
  toSumMap sofar b multiplicity = let oldKey = distSize' sofar in IntMap.insert (oldKey + multiplicity) b sofar

drawFrom :: (Alternative m, StatefulGen g m) => g -> Dist a -> m a
drawFrom g d = do
  index <- uniformRM (0, distSize d - 1) g
  let result = IntMap.lookupGE index (distChoose d)
  maybe App.empty (pure . snd) result