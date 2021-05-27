{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DrakeSpec
  ( spec,
  )
where

import Control.Comonad
  ( Comonad (duplicate, extract),
    Functor (fmap),
  )
import Data.Vector as V (replicateM)
import Drake (RingZipper (..), TorusZipper (..), rangeMod)
import Test.Hspec (Spec, describe, it, shouldBe, focus)
import Test.Hspec.Golden ()
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Positive (Positive),
    NonZero (NonZero),
    Testable (property),
  )
import Prelude (Bool, Int, pure, ($), (*), (.), div, (+), (==), (>=))

spec :: Spec
spec = do
  describe "rangeMod" $ do
    it "produces the same sign as y" . property $
      \ (x :: Int, NonZero (y :: Int)) -> (x `rangeMod` y) * y >= 0
    it "works with div (x `div` y)*y + (x `rangeMod` y) == x" . property $ 
      \ (x :: Int, NonZero (y :: Int)) -> (x `div` y)*y + (x `rangeMod` y) == x
  describe "RingZipper" $ do
    it "extract . duplicate = id" . property $
      \(x :: RingZipper Bool) -> extract (duplicate x) `shouldBe` x
    it "fmap extract . duplicate = id" . property $
      \(x :: RingZipper Bool) -> fmap extract (duplicate x) `shouldBe` x
    it "duplicate . duplicate = fmap duplicate . duplicate" . property $
      \(x :: RingZipper Bool) -> duplicate (duplicate x) `shouldBe` fmap duplicate (duplicate x)
  describe "TorusZipper" $ do
    it "extract . duplicate = id" . property $
      \(x :: TorusZipper Bool) -> extract (duplicate x) `shouldBe` x
    it "fmap extract . duplicate = id" . property $
      \(x :: TorusZipper Bool) -> fmap extract (duplicate x) `shouldBe` x
    it "duplicate . duplicate = fmap duplicate . duplicate" . property $
      \(x :: TorusZipper Bool) -> duplicate (duplicate x) `shouldBe` fmap duplicate (duplicate x)

instance Arbitrary a => Arbitrary (RingZipper a) where
  arbitrary = 
    do
      front <- arbitrary
      (Positive (width :: Int)) <- arbitrary
      vector <- V.replicateM width arbitrary
      pure RingZipper {front = front, vector = vector}

instance Arbitrary a => Arbitrary (TorusZipper a) where
  arbitrary =
    do
      frontT <- arbitrary
      (Positive (widthT :: Int)) <- arbitrary
      (Positive hight) <- arbitrary
      vectorT <- V.replicateM (widthT * hight) arbitrary
      pure TorusZipper {..}
