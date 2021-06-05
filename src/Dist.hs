{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dist (distSize, fromVector, drawFrom, Dist.fromSet, Dist (), simplify, convex, histogram, Dist.null) where

import Control.Lens hiding (index)
import Control.Applicative as App
  ( Alternative (empty),
  )
import Data.IntMap.Strict as IntMap
import Data.IntSet as IntSet
import Data.Ratio ((%))
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Vector as Vector
import Relude
import System.Random.Stateful
  ( StatefulGen,
    UniformRange (uniformRM),
  )
import Test.QuickCheck as QuickCheck hiding (scale)

newtype Dist a = Dist {toIntMap :: IntMap a}
  deriving newtype (Show, Read, Functor)

instance Eq a => Eq (Dist a) where
  a == b = f a b == f b a
    where
      f (Dist intMapX) y =
        let scale = distSize y
         in if scale > 0
              then IntMap.mapKeysWith (error "overlapping key map") (* distSize y) intMapX
              else intMapX

null :: Dist a -> Bool
null d = distSize d == 0 

distSize :: Dist a -> Int
distSize = distSize' . toIntMap

distSize' :: IntMap a -> Int
distSize' = maybe 0 fst . IntMap.lookupMax

fromSet :: forall a. Ord a => Set a -> Dist a
fromSet l = Dist $ sizeMap ^. from histogram
  where
    sizeMap :: Map a Int
    sizeMap = Set.foldl' (\m x -> Map.insertWith (+) x 1 m) mempty l

fromVector :: forall a. Ord a => Vector a -> Dist a
fromVector l = Dist $ sizeMap ^. from histogram
  where
    sizeMap :: Map a Int
    sizeMap = Vector.foldl' (\m x -> Map.insertWith (+) x 1 m) mempty l

drawFrom :: (Alternative m, StatefulGen g m) => g -> Dist a -> m a
drawFrom g d = do
  index <- uniformRM (1, distSize d) g
  let result = IntMap.lookupGE index (toIntMap d)
  maybe App.empty (pure . snd) result

histogram :: forall a. Ord a => Iso' (IntMap a) (Map a Int)
histogram = iso toH fromH
  where
    toH :: IntMap a -> Map a Int
    toH im = snd $ f (0, mempty :: Map a Int) im
    f :: (Key, Map a Int) -> IntMap a -> (Key, Map a Int)
    f = IntMap.foldlWithKey' fromSumMap
    fromSumMap :: (Key, Map a Int) -> Key -> a -> (Key, Map a Int)
    fromSumMap (top, sofar) k a = (k, Map.insertWith (+) a (k - top) sofar)
    fromH :: Map a Int -> IntMap a
    fromH = Map.foldlWithKey' toSumMap (mempty :: IntMap a)
    toSumMap :: IntMap a -> a -> Int -> IntMap a
    toSumMap sofar b multiplicity = let oldKey = distSize' sofar in IntMap.insert (oldKey + multiplicity) b sofar

convex :: Ord a => Int -> Dist a -> Int -> Dist a -> Dist a 
convex alpha a' beta b' = simplify $ Dist (stack newA newB)
  where
    a = simplify a'
    b = simplify b'
    aSize = distSize a
    bSize = distSize b
    yOverX = (aSize % bSize) / (alpha % beta)
    y = numerator yOverX
    x = denominator yOverX
    newA = IntMap.mapKeysMonotonic (* y) (toIntMap a)
    newB = IntMap.mapKeysMonotonic (* x) (toIntMap b)

stack :: Ord a => IntMap a -> IntMap a -> IntMap a
stack a b = Map.unionWith (+) (a ^. histogram) (b ^. histogram) ^. from histogram 

simplify :: Dist a -> Dist a 
simplify (Dist im) = 
  let (gcd' :: Int) = IntSet.foldl' gcd 0 (IntMap.keysSet im) in
  Dist $
    if gcd' > 1
      then IntMap.mapKeys (`div` gcd') im
      else im

instance Arbitrary a => Arbitrary (Dist a) where
  arbitrary = Dist . IntMap.mapKeys toPos <$> arbitrary
  shrink (Dist im) = 
    Dist . IntMap.mapKeys toPos <$>
      shrink (IntMap.mapKeysMonotonic (\ k -> k - 1) im)

toPos :: Key -> Key
toPos i = abs i + 1