module Main (main) where

import Control.Comonad
import Drake (TorusZipper (..), projectName)

size :: Int
size = 100

main :: IO ()
main =
  do
    putStrLn ("Executable for " ++ projectName)
    let x = TorusZipper {frontT = (5, 5), widthT = 5, vectorT = fromList [0 .. size]}
    putStrLn ("x = " ++ show (rnf x))
    let y = duplicate x
    putStrLn ("y = " ++ show (rnf y))
    let z0 = duplicate y
    putStrLn ("duplicate y = " ++ show (rnf z0))
    let z1 = fmap duplicate y
    putStrLn ("fmap duplicate y = " ++ show (rnf z0))
    putStr "z0 == z1: "
    print (z0 == z1)
