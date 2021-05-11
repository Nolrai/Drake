{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DrakeSpec
  ( spec,
  )
where

import Control.Comonad
import Data.Text hiding (filter)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Drake as D
import Test.Hspec
import Test.Hspec.Golden
import Test.QuickCheck as Q

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
    focus . it "duplicate . duplicate = fmap duplicate . duplicate" . property . withMaxSuccess 100 $
      \(x :: TorusZipper Bool) -> trace ("x: " ++ show x ++ "\n") $ duplicate (duplicate x) `shouldBe` fmap duplicate (duplicate x)

-- paste piece
-- trace ("x: " ++ show x ++ "\n") $

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fromList <$> arbitrary
  shrink v = fromList <$> shrink (toList v)

instance Arbitrary a => Arbitrary (RingZipper a) where
  arbitrary =
    do
      (Positive width) <- arbitrary
      vectorR <- fromList <$> Q.vector width
      frontR <- (`mod` width) <$> arbitrary
      pure $ RingZipper {..}

  shrink RingZipper {..} = (\v -> RingZipper {frontR = frontR `mod` V.length v, vectorR = v}) <$> filter (not . V.null) (shrink vectorR)

instance Arbitrary a => Arbitrary (TorusZipper a) where
  arbitrary = do
    (Positive hight) <- arbitrary
    (Positive widthT) <- arbitrary
    i <- (`mod` widthT) <$> arbitrary
    j <- (`mod` hight) <$> arbitrary
    vectorT <- fromList <$> Q.vector (hight * widthT)
    let frontT = (i, j)
    pure $ TorusZipper {..}

  shrink TorusZipper {..} = mkNewWidth frontT widthT (V.length vectorT) <$> filter (not . V.null) (shrink vectorT)

mkNewWidth :: (Int, Int) -> Int -> Int -> Vector a -> TorusZipper a
mkNewWidth oldFront oldWidth oldLength newV = TorusZipper {frontT = oldFront, vectorT = newV, widthT = newWidth}
  where
    newWidth = min (V.length newV) (max 1 ((V.length newV * oldLength) `div` oldWidth))
