{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DrakeSpec
  ( spec,
  )
where

import Control.Comonad
    ( Functor(fmap), (<$>), Comonad(duplicate, extract) )
import Data.Vector as V ( fromList, replicateM )
import Drake ( RingZipper(RingZipper), TorusZipper(..) )
import Prelude (Bool, pure, ($), (*), (.), (<*>), Int)
import Test.Hspec ( describe, it, shouldBe, Spec )
import Test.Hspec.Golden ()
import Test.QuickCheck
    ( Arbitrary(arbitrary), Positive(Positive), Testable(property) )

spec :: Spec
spec = do
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
  arbitrary = RingZipper <$> arbitrary <*> (V.fromList <$> arbitrary)

instance Arbitrary a => Arbitrary (TorusZipper a) where
  arbitrary =
    do
      frontT <- arbitrary
      (Positive (widthT :: Int)) <- arbitrary
      (Positive hight) <- arbitrary
      vectorT <- V.replicateM (widthT * hight) arbitrary
      pure TorusZipper {..}
