{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Drake (mkTorus)
import Draw (Draw(..))
import STCA
import Graphics.Gloss
import System.Random.Stateful
import Data.Vector as V

size :: (Int, Int)
size = (150, 150)

main :: IO ()
main = 
  do
  g <- getStdGen
  putTextLn "StdGen: "
  print g
  v <- mkCell `V.mapM` V.replicate (uncurry (*) size) ()
  let mat = mkTorus (fst size) v
  display FullScreen blue (draw (mat, ruleLHZ) size)

mkCell :: () -> IO (Cell Bool)
mkCell () = (cell <$> randomIO  <*> randomIO  <*> randomIO  <*> randomIO )