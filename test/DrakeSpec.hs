{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}

module DrakeSpec
  ( spec,
  )
where

import Control.Comonad
  ( Comonad (duplicate, extract),
    Functor (fmap),
  )
import Data.Vector as V (replicateM, take)
import Drake (Torus (..), rangeMod, rangeDivMod, torusSize, read2d)
import Control.Lens
import Control.Lens.Properties
import Test.Hspec (Spec, describe, focus, it, shouldBe)
import Test.Hspec.Golden ()
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    NonZero (NonZero),
    Positive (Positive),
    Testable (property),
  )
import Prelude (Bool, Int, div, pure, ($), (*), (+), (.), (==), (>=))

-- specialize to Bool
read2d' :: (Int, Int) -> Lens' (Torus Bool) Bool
read2d' = read2d

spec :: Spec
spec = do
  describe "rangeMod" $ do
    it "produces the same sign as y" . property $
      \(x :: Int, NonZero (y :: Int)) -> (x `rangeMod` y) * y >= 0
    it "forms an iso with unDivRangeMod" . property $ 
      \ (NonZero (y :: Int)) -> isIso (rangeDivMod y)
  describe "read2d" $ do
    it "is a setter" . property $ \ p -> isSetter (read2d' p)
    it "is a traversal" . property $ \ p -> isTraversal (read2d' p)
    it "is a lens" . property $ \ p -> isLens (read2d' p)

instance Arbitrary a => Arbitrary (Torus a) where
  arbitrary =
    do
      (Positive (widthT :: Int)) <- arbitrary
      (Positive hight) <- arbitrary
      vectorT <- V.replicateM (widthT * hight) arbitrary
      pure Torus {..}
  shrink t@Torus{vectorT = v} =
    do
      let (w, h) = torusSize t
      (Positive widthT, Positive h') <- shrink (Positive w, Positive h)
      let vectorT = V.take (widthT * h') v
      pure Torus {..}
