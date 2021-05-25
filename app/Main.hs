{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Relude as R
    ( fst,
      ($),
      fromIntegral,
      Fractional((/)),
      Monad((>>)),
      Num((*), (+), (-)),
      Ord((<), (>)),
      RealFrac(floor),
      Show,
      Applicative((<*>), pure),
      Semigroup((<>)),
      Bool(..),
      Float,
      Int,
      IO,
      Either(Right, Left),
      (<$>),
      uncurry,
      const,
      String,
      FilePath,
      stderr,
      trace,
      die,
      exitSuccess,
      readFile,
      print,
      readEither,
      show,
      ToString(toString) )
import Debug.Trace (traceIO)
import qualified Data.Vector as V
import Drake (mkTorus, TorusZipper)
import Draw (Draw (..))
import Graphics.Gloss.Interface.IO.Game as G
    ( blue,
      Display(FullScreen),
      Key(SpecialKey, MouseButton),
      Event(EventKey),
      playIO,
      KeyState(Up, Down),
      MouseButton(RightButton),
      SpecialKey(KeyEsc) )
import Graphics.Gloss.Interface.Environment (getScreenSize)
import STCA ( cell, toCell, toggleCellOnTorus, VonNeumann(..), Cell, template, Template )
import System.Random.Stateful ( randomIO, getStdGen )
import System.Environment ( getArgs )
import Data.Text.IO (hPutStrLn)
-- import Data.Map (keysSet)
import Data.Set as S ( singleton )

fullTemplate :: Template WB
fullTemplate = template (toCell (const True)) (toCell (const True))

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
    let tileSize = smallerScreenSize / (biggerMatSize + 1)
    print (size, start)
    let drawInfo = (S.singleton fullTemplate, size, tileSize)
    playIO FullScreen blue 0 start (pure . drawMat) (onEvent tileSize) updateMat

runGloss start drawInfo onEvent = playIO FullScreen blue 0 start onDraw onEvent trackUpdates
  where
    trackUpdates = traceIO "update called" >> pure mat
    onDraw state = pure $ draw drawInfo state

onEditEvent :: Float -> Event -> TorusZipper (Cell WB) -> IO (TorusZipper (Cell WB))
onEditEvent tileSize event world =
    traceIO ("Event: " <> show event) >>
      case event of
        EventKey (MouseButton RightButton) G.Down _ screenPos ->
              let cellIndex :: (Int, Int)
                  (cellIndex, vn) = toIndex tileSize screenPos in
              pure $ toggleCellOnTorus cellIndex vn world
        EventKey (SpecialKey KeyEsc) G.Up _ _ -> R.exitSuccess
        _ -> pure world

trace' :: Show a => String -> a -> a
trace' msg value = R.trace (msg <> show value) value

toIndex :: Float -> (Float, Float) -> ((Int, Int), VonNeumann)
toIndex tileSize (posX', posY') = ((tilesX, tilesY), vn)
  where
    (posX, posY) = (posX' + tileSize/2, posY' + tileSize/2)
    (tilesX, tilesY) = trace' "tiles: " (floor (posX / tileSize), floor (posY / tileSize))
    (offsetX, offsetY) = trace' "offsets: " (posX - fromIntegral tilesX * tileSize, posY - fromIntegral tilesY * tileSize)
    vn = trace' "vn: " $
      case (offsetX > offsetY, offsetY < tileSize - offsetX) of
        (True, True)    -> N
        (False, True)   -> E
        (False, False)  -> S
        (True, False)   -> W

readMatFile :: FilePath -> IO ((Int, Int), TorusZipper (Cell WB))
readMatFile filename =
  do
    x <- R.readEither <$> readFile filename
    case x of
      Left err -> R.die (R.toString err)
      Right r -> pure r

defaultSize :: (Int, Int)
defaultSize = (20, 20)

blankTorus :: ((Int, Int), TorusZipper (Cell WB))
blankTorus = (defaultSize, mkTorus (fst defaultSize) underlyingVector)
  where
  underlyingVector = V.replicate (uncurry (*) defaultSize) blankCell
  blankCell = toCell (const False)

mkRandomTorus :: IO ((Int, Int), TorusZipper (Cell WB))
mkRandomTorus =
  do
    g <- getStdGen
    hPutStrLn stderr $ "StdGen: " <> show g
    v <- mkCell `V.mapM` V.replicate (uncurry (*) defaultSize) ()
    pure (defaultSize, mkTorus (fst defaultSize) v)

mkCell :: () -> IO (Cell WB)
mkCell () = cell <$> randomIO <*> randomIO <*> randomIO <*> randomIO
