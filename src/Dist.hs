{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dist (distSize, fromVector, drawFrom, fromSet, Dist ()) where

import Control.Applicative as App
  ( Alternative (empty),
    Applicative (pure),
  )
import Data.IntMap as IntMap
  ( IntMap,
    insert,
    lookupGE,
    lookupMax,
    mapKeysWith,
  )
import Data.Map as Map (Map, foldlWithKey', insertWith)
import Data.Set as Set
import Data.Vector as Vector
import Relude
  ( Eq ((==)),
    Functor,
    Int,
    Monoid (mempty),
    Num ((*), (+)),
    Ord,
    Read,
    Show,
    error,
    fst,
    maybe,
    snd,
    (.),
    (>),
  )
import System.Random.Stateful
  ( StatefulGen,
    UniformRange (uniformRM),
  )

newtype Dist a = Dist {toIntMap :: IntMap a}
  deriving newtype (Show, Read, Functor)

instance Eq a => Eq (Dist a) where
  a == b = f a b == f b a
    where
      f (Dist intMapX) y =
        let scale = distSize y
         in if scale > 0
              then mapKeysWith (error "overlapping key map") (* distSize y) intMapX
              else intMapX

distSize :: Dist a -> Int
distSize = distSize' . toIntMap

distSize' :: IntMap b -> Int
distSize' = maybe 0 fst . IntMap.lookupMax

fromSet :: forall a. Ord a => Set a -> Dist a
fromSet l = fromSizeMap sizeMap
  where
    sizeMap :: Map a Int
    sizeMap = Set.foldl' (\m x -> Map.insertWith (+) x 1 m) mempty l

fromVector :: forall a. Ord a => Vector a -> Dist a
fromVector l = fromSizeMap sizeMap
  where
    sizeMap :: Map a Int
    sizeMap = Vector.foldl' (\m x -> Map.insertWith (+) x 1 m) mempty l

fromSizeMap :: forall a. Map a Int -> Dist a
fromSizeMap m = Dist (toIntMapF m)
  where
    toIntMapF :: Map a Int -> IntMap a
    toIntMapF = Map.foldlWithKey' toSumMap (mempty :: IntMap a)
    toSumMap :: IntMap b -> b -> Int -> IntMap b
    toSumMap sofar b multiplicity = let oldKey = distSize' sofar in IntMap.insert (oldKey + multiplicity) b sofar

drawFrom :: (Alternative m, StatefulGen g m) => g -> Dist a -> m a
drawFrom g d = do
  index <- uniformRM (1, distSize d) g
  let result = IntMap.lookupGE index (toIntMap d)
  maybe App.empty (pure . snd) result
