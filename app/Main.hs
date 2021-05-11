module Main (main) where

import Drake ()
import Draw ()
import Graphics.Gloss


main :: IO ()
main = display FullScreen white (Circle 80)