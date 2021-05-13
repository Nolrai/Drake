{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Vector as V
import Drake (mkTorus)
import Draw (Draw (..))
import Graphics.Gloss
import STCA
import System.Random.Stateful

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
mkCell () = cell <$> randomIO <*> randomIO <*> randomIO <*> randomIO
