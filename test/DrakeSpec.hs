{-# LANGUAGE ScopedTypeVariables #-}

module DrakeSpec
  ( spec,
  )
where

import Control.Comonad
import Data.Text
import Drake
import Test.Hspec
import Test.Hspec.Golden
import Test.QuickCheck

spec = do
  describe "RingZipper" $ do
    it "extract . duplicate = id" . property $
      \(x :: RingZipper Bool) -> (extract $ duplicate x) `shouldBe` x
    it "fmap extract . duplicate = id" . property $
      \(x :: RingZipper Bool) -> (fmap extract $ duplicate x) `shouldBe` x
    it "duplicate . duplicate = fmap duplicate . duplicate" . property $
      \(x :: RingZipper Bool) -> (duplicate $ duplicate x) `shouldBe` (fmap duplicate $ duplicate x)
  describe "TorusZipper" $ do
    it "extract . duplicate = id" . property $
      \(x :: TorusZipper Bool) -> (extract $ duplicate x) `shouldBe` x
    it "fmap extract . duplicate = id" . property $
      \(x :: TorusZipper Bool) -> (fmap extract $ duplicate x) `shouldBe` x
    it "duplicate . duplicate = fmap duplicate . duplicate" . property $
      \(x :: TorusZipper Bool) -> (duplicate $ duplicate x) `shouldBe` (fmap duplicate $ duplicate x)
