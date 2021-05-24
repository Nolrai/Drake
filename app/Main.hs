{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import qualified Data.Vector as V
import Drake (mkTorus, TorusZipper)
import Draw (Draw (..), tileSize)
import Graphics.Gloss.Interface.IO.Game as G
import STCA
import System.Random.Stateful
import System.Environment
import Data.Text.IO (hPutStrLn)
import Data.Map (keysSet)

main :: IO ()
main =
  do
    args <- getArgs
    (size, start) <- 
      case args of
        [filename] -> readMatFile filename
        _ -> pure blankTorus
    print (size, start)
    playIO FullScreen blue 0 start (\mat -> pure $ draw (mat, keysSet lhz_map) size) onEvent (\ _ mat -> trace "update called" $ pure mat)

onEvent :: Event -> TorusZipper (Cell Bool) -> IO (TorusZipper (Cell Bool)) 
onEvent event world =
    trace ("Event: " <> show event) $
      case event of
        EventKey (MouseButton RightButton) G.Down _ screenPos ->
              let cellIndex :: (Int, Int)
                  (cellIndex, vn) = toIndex screenPos in
              pure $ toggleCellOnTorus cellIndex vn world
        EventKey (SpecialKey KeyEsc) G.Up _ _ -> exitSuccess
        _ -> pure world

trace' :: Show a => String -> a -> a
trace' msg value = trace (msg <> show value) value

toIndex :: (Float, Float) -> ((Int, Int), VN)
toIndex (posX', posY') = ((tilesX, tilesY), vn)
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

blankTorus :: ((Int, Int), TorusZipper (Cell Bool))
blankTorus = (defaultSize, mkTorus (fst defaultSize) underlyingVector)
  where
  underlyingVector = V.replicate (uncurry (*) defaultSize) blankCell
  blankCell = toCell (\ _ -> False)

mkRandomTorus :: IO ((Int, Int), TorusZipper (Cell Bool))
mkRandomTorus =
  do
    g <- getStdGen
    hPutStrLn stderr $ "StdGen: " <> show g
    v <- mkCell `V.mapM` V.replicate (uncurry (*) defaultSize) ()
    pure $ (defaultSize, mkTorus (fst defaultSize) v)
    
mkCell :: () -> IO (Cell Bool)
mkCell () = cell <$> randomIO <*> randomIO <*> randomIO <*> randomIO
