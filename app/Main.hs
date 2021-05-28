{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

-- import Data.Map (keysSet)
import Data.Set as S (singleton)
import Data.Text.IO (hPutStrLn)
import qualified Data.Vector as V
import Debug.Trace (traceIO)
import Drake (Torus)
import Draw (Draw (..))
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game as G
  ( Display (FullScreen),
    Event (EventKey),
    Key (MouseButton, SpecialKey),
    KeyState (Down, Up),
    MouseButton (RightButton),
    Picture,
    SpecialKey (KeyEsc),
    blue,
    playIO,
  )
import Relude as R
  ( Applicative (pure, (<*>)),
    Bool (..),
    Either (Left, Right),
    FilePath,
    Float,
    Fractional ((/)),
    IO,
    Int,
    Monad ((>>)),
    Num ((*), (+), (-)),
    Ord ((<), (>)),
    RealFrac (floor),
    Semigroup ((<>)),
    Show,
    String,
    ToString (toString),
    const,
    die,
    exitSuccess,
    fromIntegral,
    fst,
    max,
    min,
    print,
    readEither,
    readFile,
    show,
    stderr,
    trace,
    uncurry,
    ($),
    (<$>),
    (<>),
  )
import STCA (Cell, GreaterCell, RedBlack (..), VonNeumann (..), cell, template, toCell, toggleCellOnTorus)
import System.Environment (getArgs)
import System.Random.Stateful (getStdGen, randomIO)

blackGreaterCell :: GreaterCell RedBlack
blackGreaterCell = template (toCell (const Black)) (toCell (const Black))

main :: IO ()
main =
  do
    args <- getArgs
    (size, start) <-
      case args of
        ["random"] -> mkRandomTorus
        [filename] -> readMatFile filename
        _ -> pure blankTorus
    screenSize <- getScreenSize
    let smallerScreenSize = uncurry min screenSize
    let biggerMatSize = uncurry max size
    let tileSize :: Float
        tileSize = (fromIntegral smallerScreenSize / fromIntegral (biggerMatSize + 1))
    print (size, tileSize, start)
    let drawInfo = (S.singleton blackGreaterCell, size, tileSize)
    runGloss start drawInfo onEditEvent

runGloss :: forall world drawInfo. Draw world drawInfo => world -> drawInfo -> (drawInfo -> Event -> world -> IO world) -> IO ()
-- runGloss :: Torus (Cell RedBlack) -> (Set (GreaterCell RedBlack),  (Int, Int), Float) -> ((Set (GreaterCell RedBlack),  (Int, Int), Float) -> Even -> Torus (Cell RedBlack) -> IO world) -> IO ()
runGloss start drawInfo onEvent = playIO FullScreen blue 0 start onDraw (onEvent drawInfo) trackUpdates
  where
    trackUpdates :: Float -> world -> IO world
    trackUpdates stepSize mat = traceIO ("update called with step size of " <> show stepSize) >> pure mat
    onDraw :: world -> IO G.Picture
    onDraw state = pure $ draw drawInfo state

onEditEvent :: (a, b, Float) -> Event -> Torus (Cell RedBlack) -> IO (Torus (Cell RedBlack))
onEditEvent (_highlight, _matrixSize, tileSize) event world =
  traceIO ("Event: " <> show event)
    >> case event of
      EventKey (MouseButton RightButton) G.Down _ screenPos ->
        let cellIndex :: (Int, Int)
            (cellIndex, vn) = toIndex tileSize screenPos
         in pure $ toggleCellOnTorus cellIndex vn world
      EventKey (SpecialKey KeyEsc) G.Up _ _ -> R.exitSuccess
      _ -> pure world

trace' :: Show a => String -> a -> a
trace' msg value = R.trace (msg <> show value) value

toIndex :: Float -> (Float, Float) -> ((Int, Int), VonNeumann)
toIndex tileSize (posX', posY') = ((tilesX, tilesY), vn)
  where
    (posX, posY) = (posX' + tileSize / 2, posY' + tileSize / 2)
    (tilesX, tilesY) = trace' "tiles: " (floor (posX / tileSize), floor (posY / tileSize))
    (offsetX, offsetY) = trace' "offsets: " (posX - fromIntegral tilesX * tileSize, posY - fromIntegral tilesY * tileSize)
    vn = trace' "vn: " $
      case (offsetX > offsetY, offsetY < tileSize - offsetX) of
        (True, True) -> N
        (False, True) -> E
        (False, False) -> S
        (True, False) -> W

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
blankTorus = (defaultSize, Torus (fst defaultSize) underlyingVector)
  where
    underlyingVector = V.replicate (uncurry (*) defaultSize) blankCell
    blankCell = toCell (const Red)

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
