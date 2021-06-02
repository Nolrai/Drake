{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
    redGreaterCell,
  )
where

-- import Data.Map (keysSet)

import Control.Lens ((^.))
import Data.Text.IO (hPutStrLn)
import qualified Data.Vector as V
import Debug.Trace (traceIO)
import Drake (Torus (..))
import Draw (Draw (..))
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game as G
  ( Display (FullScreen),
    Event (EventKey),
    Key (MouseButton, SpecialKey),
    KeyState (..),
    MouseButton (RightButton),
    Picture,
    SpecialKey (KeyEsc),
    blue,
    playIO,
  )
import Relude as R hiding (state)
import STCA (Cell, GreaterCell, RedBlack (..), VonNeumann (..), cell, greaterCell, toCell, toggleSubCellOnTorus, greaterCellFromTorus, lhzMap, TorusEx)
import System.Environment (getArgs)
import System.Random.Stateful (getStdGen, randomIO)
import Data.List as L ( isInfixOf, elem )
import Data.Map as M (keysSet)
-- import Data.Set as S (singleton)


blankCell :: Cell RedBlack
blankCell = const Red ^. toCell

redGreaterCell :: GreaterCell RedBlack
redGreaterCell = (blankCell, blankCell) ^. greaterCell

testGC :: GreaterCell RedBlack
testGC = (\b->if b then Black else Red) <$> ((== N) ^. toCell, (`L.elem` [N, E]) ^. toCell) ^. greaterCell

main :: IO ()
main =
  do
    args <- getArgs
    (size, start) <-
      case args of
        ["random"]  -> mkRandomTorus
        ['c':str] -> pure $ strTorus str
        ["f", filename]  -> readMatFile filename
        _ -> pure blankTorus
    screenSize <- getScreenSize
    let smallerScreenSize = uncurry min screenSize
    let biggerMatSize = uncurry max size
    let tileSize :: Float
        tileSize = fromIntegral smallerScreenSize / fromIntegral (biggerMatSize + 1)
    print (size, tileSize, start)
    let drawInfo = (M.keysSet lhzMap, size, tileSize)
    runGloss (mkTorusEx start) drawInfo onEditEvent

data World = World {_board :: TorusEx, _controlState :: ControlState}

runGloss :: forall world drawInfo. 
  Draw world drawInfo => 
    world -> drawInfo -> (drawInfo -> Event -> world -> IO world) -> IO ()
runGloss start drawInfo onEvent = playIO FullScreen blue 1 start onDraw (onEvent drawInfo) trackUpdates
  where
    trackUpdates :: Float -> world -> IO world
    trackUpdates stepSize mat = traceIO ("update called with step size of " <> show stepSize) >> pure mat
    onDraw :: world -> IO G.Picture
    onDraw state = pure $ draw drawInfo state

onEditEvent :: (a, b, Float) -> Event -> Torus (Cell RedBlack) -> IO (Torus (Cell RedBlack))
onEditEvent (_highlight, _matrixSize, tileSize) event world =
  traceIO ("Event: " <> show event)
    >> case event of
      EventKey (MouseButton RightButton) G.Up _ screenPos ->
        let cellIndex :: (Int, Int)
            (cellIndex, vn) = toIndex tileSize screenPos
         in do
           let result = toggleSubCellOnTorus cellIndex vn world
           traceIO ("New GC is: " <> show (result ^. greaterCellFromTorus cellIndex))
           pure result
      EventKey (SpecialKey KeyEsc) G.Up _ _ -> R.exitSuccess
      _ -> pure world

trace' :: Show a => String -> a -> a
trace' msg value = R.trace (msg <> show value) value

toIndex :: Float -> (Float, Float) -> ((Int, Int), VonNeumann)
toIndex tileSize (posX', posY') = ((tilesX, tilesY), vn)
  where
    (posX, posY) = (posX' + tileSize / 2, posY')
    (tilesX, tilesY) = trace' "tiles: " (floor (posX / tileSize), floor (posY / tileSize))
    (offsetX, offsetY) = trace' "offsets: " (posX - fromIntegral tilesX * tileSize, posY - fromIntegral tilesY * tileSize)
    vn = trace' "vn: " $
      case (offsetX > offsetY, offsetY < tileSize - offsetX) of
        (False, False) -> N
        (True, False) -> E
        (True, True) -> S
        (False, True) -> W

readMatFile :: FilePath -> IO ((Int, Int), Torus (Cell RedBlack))
readMatFile filename =
  do
    x <- R.readEither <$> readFile filename
    case x of
      Left err -> R.die (R.toString err)
      Right r -> pure r

defaultSize :: (Int, Int)
defaultSize = (20, 20)

blankTorus :: ((Int, Int), Torus (Cell RedBlack))
blankTorus = defaultTorus blankCell

strTorus :: String -> ((Int, Int), Torus (Cell RedBlack))
strTorus str = defaultTorus ((\vn -> if show vn `isInfixOf` str then Black else Red) ^. toCell)

defaultTorus :: Cell RedBlack -> ((Int, Int), Torus (Cell RedBlack))
defaultTorus c = (defaultSize, Torus (fst defaultSize) underlyingVector)
  where
    underlyingVector = V.replicate (uncurry (*) defaultSize) c 

mkRandomTorus :: IO ((Int, Int), Torus (Cell RedBlack))
mkRandomTorus =
  do
    g <- getStdGen
    hPutStrLn stderr $ "StdGen: " <> show g
    v <- mkCell `V.mapM` V.replicate (uncurry (*) defaultSize) ()
    pure (defaultSize, Torus (fst defaultSize) v)

mkCell :: () -> IO (Cell RedBlack)
mkCell () = cell <$> redBlackIO <*> redBlackIO <*> redBlackIO <*> redBlackIO
  where
    redBlackIO = (\b -> if b then Black else Red) <$> randomIO
