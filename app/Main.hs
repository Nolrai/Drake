{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import qualified Data.Vector as V
import Drake (mkTorus, TorusZipper)
import Draw (Draw (..))
import Graphics.Gloss
import STCA
import System.Random.Stateful
import System.Environment
import Data.Text.IO (hPutStrLn)

main :: IO ()
main =
  do
    args <- getArgs
    (size, mat) <- 
      case args of
        [filename] -> readMatFile filename
        _ -> mkRandomTorus
    print (size, mat)
    display FullScreen blue (draw (mat, ruleLHZ) size)

readMatFile :: FilePath -> IO ((Int, Int), TorusZipper (Cell Bool))
readMatFile filename = 
  do
    x <- readEither <$> readFile filename
    case x of
      Left err -> fail (toString err)
      Right r -> pure r


defaultSize :: (Int, Int)
defaultSize = (150, 150)

mkRandomTorus :: IO ((Int, Int), TorusZipper (Cell Bool))
mkRandomTorus =
  do
    g <- getStdGen
    hPutStrLn stderr $ "StdGen: " <> show g
    v <- mkCell `V.mapM` V.replicate (uncurry (*) defaultSize) ()
    pure $ (defaultSize, mkTorus (fst defaultSize) v)
    
mkCell :: () -> IO (Cell Bool)
mkCell () = cell <$> randomIO <*> randomIO <*> randomIO <*> randomIO
