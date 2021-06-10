{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module STCA.RulesSpec
  ( spec,
    repeated,
    nubSort,
  )
where

import Control.Lens (ALens', Iso', Lens', from)
import Control.Lens.Properties (isIso, isLens, isSetter, isTraversal)
import Data.List (isSubsequenceOf)
import Data.Set as Set
import STCA.Cell (Cell ())
import STCA.CellSpec ()
import STCA.GreaterCell
import STCA.Rules (Body (..), LAR (..), LhsTemplate (), RedBlack (..), RhsTemplate (), lhzBase, mkLHS, mkRHS, readBody, rotateLar, toBody, toHead, toggle, vnDiff)
import STCA.Direction (Direction)
import STCA.DirectionSpec ()
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList, shouldNotBe, shouldSatisfy)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function, NonEmptyList (..), genericShrink, property, (.&&.))
import Test.QuickCheck.Gen as QG
import Prelude

mirror :: LAR -> LAR
mirror L = R
mirror A = A
mirror R = L

spec :: Spec
spec = do
  describe "Lar" $ do
    describe "rotateLar" $ do
      it "is root of involution" . property $
        \lar vn -> let f = rotateLar lar in (f . f . f . f) vn `shouldBe` vn
      it "is derangement" . property $
        \lar vn -> rotateLar lar vn `shouldNotBe` vn
      it " . mirror is inverse of rotateLar" . property $
        \lar vn -> rotateLar (mirror lar) (rotateLar lar vn) `shouldBe` vn
      it " is inverse of rotateLar . mirror" . property $
        \lar vn -> rotateLar lar (rotateLar (mirror lar) vn) `shouldBe` vn
    describe "mirror" $ do
      it "is involution" . property $
        \lar -> mirror (mirror lar) `shouldBe` lar
    describe "vnDiff" $ do
      it "is converse of rotateLar" . property $
        \vn lar -> (vn `vnDiff` rotateLar lar vn) `shouldBe` Just lar
      it "recognizes equality" . property $
        \vn -> vn `vnDiff` vn `shouldBe` Nothing
  describe "RedBlack" $ do
    describe "toggle" $ do
      it "is an involution" . property $
        \rb -> toggle (toggle rb) `shouldBe` rb
      it "is a derangement" . property $
        \rb -> toggle rb `shouldNotBe` rb
  describe "Body" $
    describe "readBody" $ do
      it "is an setter" . property $
        \lar -> isSetter (readBody lar :: Lens' (Body RedBlack) RedBlack)
      it "is an traversal" . property $
        \lar -> isTraversal (readBody lar :: Lens' (Body RedBlack) RedBlack)
      it "is an lens" . property $
        \lar -> isLens (readBody lar :: Lens' (Body RedBlack) RedBlack)
  describe "LhsTemplate" $ do
    describe "toHead" $ do
      it "is an setter" . property $ isSetter (toHead :: Lens' LhsTemplate Direction)
      it "is an traversal" . property $ isTraversal (toHead :: Lens' LhsTemplate Direction)
      it "is an lens" . property $ isLens (toHead :: Lens' LhsTemplate Direction)
    describe "toBody" $ do
      it "is an setter" . property $ isSetter (toBody :: Lens' LhsTemplate (Body RedBlack))
      it "is an traversal" . property $ isTraversal (toBody :: Lens' LhsTemplate (Body RedBlack))
      it "is an lens" . property $ isLens (toBody :: Lens' LhsTemplate (Body RedBlack))
  describe "RhsTemplate" $ do
    describe "toHead" $ do
      it "is an setter" . property $ isSetter (toHead :: Lens' RhsTemplate LAR)
      it "is an traversal" . property $ isTraversal (toHead :: Lens' RhsTemplate LAR)
      it "is an lens" . property $ isLens (toHead :: Lens' RhsTemplate LAR)
    describe "toBody" $ do
      it "is an setter" . property $ isSetter (toBody :: Lens' RhsTemplate (Body RedBlack))
      it "is an traversal" . property $ isTraversal (toBody :: Lens' RhsTemplate (Body RedBlack))
      it "is an lens" . property $ isLens (toBody :: Lens' RhsTemplate (Body RedBlack))
  describe "lhzBase" $ do
    it "is forward deterministic" . property $
      repeated (fst <$> lhzBase) `shouldBe` []
    it "is backward deterministic" . property $
      repeated (snd <$> lhzBase) `shouldBe` []
  describe "for testing" $ do
    describe "repeated" $ do
      it " plus nubSort shoud match original" . property $
        \(l :: [Int]) -> (repeated l ++ nubSort l) `shouldMatchList` l
      it "is a (not nessarily contigous) (reversed) sublist of" . property $
        \(l :: [Int]) -> reverse (repeated l) `shouldSatisfy` (`isSubsequenceOf` l)
      it "is shorter" . property $
        \(NonEmpty (l :: [Int])) -> (length . repeated) l `shouldSatisfy` (< length l)
    describe "nubSort" $ do
      it "is idempotent" . property $
        \l -> nubSort (nubSort l) `shouldBe` nubSort l
      it "is not longer" . property $
        \l -> (length . nubSort) l `shouldSatisfy` (<= length l)
      it "preserves membership" . property $
        \l -> let nl = nubSort l in all (`elem` l) nl && all (`elem` nl) l
      it "has no repeated values" . property $
        \l -> repeated (nubSort l) `shouldBe` []

nubSort :: [Int] -> [Int]
nubSort = Set.toList . Set.fromList

instance Arbitrary LAR where
  arbitrary = QG.elements [L, A, R]

  shrink L = []
  shrink A = [L]
  shrink R = [A, L]

instance CoArbitrary LAR

instance Function LAR

instance Arbitrary RedBlack where
  arbitrary = QG.elements [Red, Black]

  shrink Red = []
  shrink Black = [Red]

instance CoArbitrary RedBlack

instance Function RedBlack

repeated :: Ord a => [a] -> [a]
repeated = go mempty mempty
  where
    go :: Ord a => [a] -> Set a -> [a] -> [a]
    go repeatedElements _ [] = repeatedElements
    go r seen (x : xs) = go (if x `Set.member` seen then x : r else r) (Set.insert x seen) xs

instance Arbitrary (Body RedBlack) where
  arbitrary = Body <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary LhsTemplate where
  arbitrary = mkLHS <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary RhsTemplate where
  arbitrary = mkRHS <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary (Body RedBlack)

instance CoArbitrary LhsTemplate

instance CoArbitrary RhsTemplate

instance Function (Body RedBlack)

instance Function LhsTemplate

instance Function RhsTemplate
