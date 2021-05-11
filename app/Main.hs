module Main (main) where

import Drake ()
import Draw ()
import Graphics.Gloss


main :: IO ()
main = displace FullScreen white (Circle 80)
