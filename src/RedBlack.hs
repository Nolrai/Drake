{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module RedBlack (RedBlack (..), toggle) where
import Relude (Eq, Ord, Show, Read)
import GHC.Generics

data RedBlack
  = Red -- Empty
  | Black -- Full
  deriving stock (Eq, Ord, Show, Read, Generic)

toggle :: RedBlack -> RedBlack
toggle Red = Black
toggle Black = Red