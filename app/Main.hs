{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
    redGreaterCell,
  )
where

import Control.Arrow ()
import Control.Lens hiding (index)
import Data.List as L (elem, isInfixOf)
import Data.Map as M (keysSet)
import Data.Set as Set (delete, insert)
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
    SpecialKey (KeyEsc, KeySpace),
    blue,
    playIO,
  )
import Relude as R
import STCA
import System.Environment (getArgs)
import System.Random.Stateful

-- import Data.Set as S (singleton)

blankCell :: Cell RedBlack
blankCell = const Red ^. toCell

redGreaterCell :: GreaterCell RedBlack
redGreaterCell = (blankCell, blankCell) ^. greaterCell

testGC :: GreaterCell RedBlack
testGC = (\b -> if b then Black else Red) <$> ((== N) ^. toCell, (`L.elem` [N, E]) ^. toCell) ^. greaterCell

newtype ControlState = ControlState {_runControlState :: AtomicGenM StdGen}

makeLenses ''ControlState

data World = World {_board :: TorusEx, _controlState :: ControlState}

makeLenses ''World

gen :: Lens' World (AtomicGenM StdGen)
gen = controlState . runControlState

main :: IO ()
main =
  do
    args <- getArgs
    (size, startingTorus) <-
      case args of
        ["random"] -> mkRandomTorus
        ['c' : str] -> pure $ strTorus str
        ["f", filename] -> readMatFile filename
        _ -> pure blankTorus
    screenSize <- getScreenSize
    let smallerScreenSize = uncurry min screenSize
    let biggerMatSize = uncurry max size
    let tileSize :: Float
        tileSize = fromIntegral smallerScreenSize / fromIntegral (biggerMatSize + 1)
    print (size, tileSize, startingTorus)
    let drawInfo = (M.keysSet lhzMap, size, tileSize)
    atomicGen <- newAtomicGenM =<< getStdGen
    let startingWorld = World (mkTorusEx startingTorus) (ControlState atomicGen)
    runGloss drawInfo startingWorld (board . torus) onEditEvent

runGloss ::
  forall world drawable drawInfo.
  Draw drawable drawInfo =>
  drawInfo ->
  world ->
  Getter world drawable ->
  (drawInfo -> Event -> world -> IO world) ->
  IO ()
runGloss drawInfo start toDrawable onEvent = playIO FullScreen blue 1 start onDraw (onEvent drawInfo) trackUpdates
  where
    trackUpdates :: Float -> world -> IO world
    trackUpdates stepSize mat = traceIO ("update called with step size of " <> show stepSize) >> pure mat
    onDraw :: world -> IO G.Picture
    onDraw world = pure $ draw drawInfo (world ^. toDrawable)

onEditEvent :: (a, b, Float) -> Event -> World -> IO World
onEditEvent (_highlight, _matrixSize, tileSize) event world =
  do
    (traceIO ("Event: " <> show event) :: IO ())
    case event of
      EventKey (MouseButton RightButton) G.Up _ screenPos ->
        handleMouseButtonUp (toIndex tileSize screenPos) world
      EventKey (SpecialKey KeyEsc) G.Up _ _ -> R.exitSuccess
      EventKey (SpecialKey KeySpace) G.Up _ _ -> updateWorld world
      _ -> pure world

updateWorld :: World -> IO World
updateWorld = execStateT updateWorld'

updateWorld' ::
  forall (m :: Type -> Type).
  (Alternative m, MonadIO m) =>
  StateT World m ()
updateWorld' =
  do
    (g :: AtomicGenM StdGen) <- use gen
    (newBoards :: Int -> TorusEx) <- uses board wideStep
    (new :: TorusEx) <- newBoards <$> uniformM g
    board .= new

handleMouseButtonUp :: ((Int, Int), VonNeumann) -> World -> IO World
handleMouseButtonUp index = execStateT (handleMouseButtonUp' index)

-- Its a little weird to use StateT for a single function like this..but it helped it so much.
-- I maybe could use pair list instead..hmm.
handleMouseButtonUp' :: ((Int, Int), VonNeumann) -> StateT World IO ()
handleMouseButtonUp' (cellIndex, vn) =
  do
    liftIO . traceIO $ "toggle at " <> show (cellIndex, vn)
    board . torus %= toggleSubCellOnTorus cellIndex vn
    mapM_ updateHeadAt [cellIndex, cellIndex `offset` vn]

updateHeadAt :: (Int, Int) -> StateT World IO ()
updateHeadAt cellIndex =
  do
    isHead <- use (board . isLhzHead cellIndex)
    liftIO . traceIO $ show cellIndex <> if isHead then " is a head now" else " isn't a head."
    board . headSet %= (if isHead then Set.insert else Set.delete) cellIndex

-- for debug
-- trace' :: Show a => String -> a -> a
-- trace' msg value = R.trace (msg <> show value) value

-- get the index into the sub cell the mouse is over
-- note that because of how Torus works this still "works" even if the mouse is not over the matrix.
toIndex :: Float -> (Float, Float) -> ((Int, Int), VonNeumann)
toIndex tileSize (posX', posY') = ((tilesX, tilesY), vn)
  where
    (posX, posY) = (posX' + tileSize / 2, posY')
    (tilesX, tilesY) = (floor (posX / tileSize), floor (posY / tileSize))
    (offsetX, offsetY) = (posX - fromIntegral tilesX * tileSize, posY - fromIntegral tilesY * tileSize)
    vn =
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
