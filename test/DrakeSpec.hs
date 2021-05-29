{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DrakeSpec
  ( spec,
  )
where

import Control.Lens (Lens', from, (^.))
import Control.Lens.Properties
import Control.Monad (guard)
import Data.Vector as V (mapM, replicateM, take)
import Drake (Torus (..), rangeDivMod, rangeMod, read2d, torusSize)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Golden ()
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    NonZero (NonZero),
    Positive (Positive),
    Testable (property),
  )
import Prelude (Bool, Int, pure, ($), (&&), (*), (+), (.), (>=))

-- specialize to Bool
read2d' :: (Int, Int) -> Lens' (Torus Bool) Bool
read2d' = read2d

spec :: Spec
spec = do
  describe "rangeMod" $ do
    it "produces the same sign as y" . property $
      \(x :: Int, NonZero (y :: Int)) -> (x `rangeMod` y) * y >= 0
    it "forms an left partial iso" . property $
      \(NonZero (y :: Int)) (x :: Int) -> (x ^. rangeDivMod y . from (rangeDivMod y)) `shouldBe` x
  describe "read2d" $ do
    it "is a setter" . property $ \p -> isSetter (read2d' p)
    it "is a traversal" . property $ \p -> isTraversal (read2d' p)
    it "is a lens" . property $ \p -> isLens (read2d' p)

instance Arbitrary a => Arbitrary (Torus a) where
  arbitrary =
    do
      (Positive (widthT' :: Int)) <- arbitrary
      (Positive hight') <- arbitrary
      let (widthT, hight) = (widthT' + 2, hight' + 2)
      vectorT <- V.replicateM (widthT * hight) arbitrary
      pure Torus {..}
  shrink t@Torus {vectorT = v} =
    do
      let (w, h) = torusSize t
      (Positive widthT, Positive h') <- shrink (Positive w, Positive h)
      guard (w >= 3 && h >= 3)
      vectorT <- V.mapM (\x -> x : shrink x) $ V.take (widthT * h') v
      pure Torus {..}
