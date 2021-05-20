{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import qualified Data.Vector as V
import Drake (mkTorus, TorusZipper)
import Draw (Draw (..), tileSize)
import Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Interface.Environment as G
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
        EventKey (MouseButton RightButton) G.Down _ screenPos ->
            do
              screenSize <- G.getScreenSize
              let cellIndex :: (Int, Int)
                  (cellIndex, vn) = toIndex screenSize screenPos
              pure $ toggleCellOnTorus cellIndex vn world
        EventKey (SpecialKey KeyEsc) G.Up _ _ -> exitSuccess
        _ -> pure world

trace' msg value = trace (msg <> show value) value

toIndex :: (Int, Int) -> (Float, Float) -> ((Int, Int), VN)
toIndex (screenSizeX, screenSizeY) (posX', posY') = ((tilesX, tilesY), vn)
  where
    (posX, posY) = (posX' + tileSize/2, posY' + tileSize/2)
    (tilesX, tilesY) = trace' "tiles: " (floor (posX / tileSize), floor (posY / tileSize))
    (offsetX, offsetY) = trace' "offsets: " (posX - (fromIntegral tilesX * tileSize), posY - (fromIntegral tilesY * tileSize))
    vn = trace' "vn: " $ 
      case (offsetX > offsetY, offsetY < tileSize - offsetX) of
        (True, True)    -> N
        (False, True)   -> E
        (False, False)  -> S
        (True, False)   -> W

readMatFile :: FilePath -> IO ((Int, Int), TorusZipper (Cell Bool))
readMatFile filename = 
  do
    x <- readEither <$> readFile filename
    case x of
      Left err -> die (toString err)
      Right r -> pure r

defaultSize :: (Int, Int)
defaultSize = (20, 20)

mkRandomTorus :: IO ((Int, Int), TorusZipper (Cell Bool))
mkRandomTorus =
  do
    g <- getStdGen
    hPutStrLn stderr $ "StdGen: " <> show g
    v <- mkCell `V.mapM` V.replicate (uncurry (*) defaultSize) ()
    pure $ (defaultSize, mkTorus (fst defaultSize) v)
    
mkCell :: () -> IO (Cell Bool)
mkCell () = cell <$> randomIO <*> randomIO <*> randomIO <*> randomIO
