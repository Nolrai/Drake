{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Drake (mkTorus)
import Draw (Draw(..))
import STCA
import Graphics.Gloss
import System.Random.Stateful
import Data.Vector as V

main :: IO ()
main = 
  do
  g <- getStdGen
  putTextLn "StdGen: "
  print g
  v <- mkCell `V.mapM` V.replicate 200 ()
  let mat = mkTorus 20 v
  display FullScreen blue (draw mat (20, 20)) 

mkCell :: () -> IO (Cell Bool)
mkCell () = (cell <$> randomIO  <*> randomIO  <*> randomIO  <*> randomIO )