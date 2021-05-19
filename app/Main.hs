{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import qualified Data.Vector as V
import Drake (mkTorus, TorusZipper)
import Draw (Draw (..), tileSize)
import Graphics.Gloss.Interface.Pure.Game as G
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
    playIO FullScreen blue 0 mat (\mat -> pure $ draw (mat, ruleLHZ) size) onEvent (\ _ mat -> trace "update called" $ pure mat)

onEvent :: Event -> TorusZipper (Cell Bool) -> IO (TorusZipper (Cell Bool)) 
onEvent event world =
    trace ("Event: " <> show event) $
      case event of
        EventKey (MouseButton RightButton) G.Down _ pos ->
            do
              screenSize <- getScreenSize
              let (pos, vn) = toIndex screenSize pos
              pure $ toggleCellOnTorus pos vn world

toIndex :: (Int, Int) -> (Float, Float) -> ((Int, Int), VN)
toIndex (screenSizeX, screenSizeY) (posX', posY') = ((tilesX, tilesY), vn)
  where
    (posX, posY) = (posX' + (fromIntegral screenSizeX)/2, posY' + (fromIntegral screenSizeY)/2)
    (tilesX, tilesY) = (floor (posX / tileSize), floor (posY / tileSize))
    (offsetX, offsetY) = (posX - (fromIntegral tilesX * tileSize), posY - (fromIntegral tilesY * tileSize))
    vn = case (offsetX > offsetY, offsetY < tileSize - offsetX) of
      (True, True)    -> N
      (True, False)   -> E
      (False, False)  -> S
      (False, True)   -> W

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
