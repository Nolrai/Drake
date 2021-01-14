module Main (main) where

import Drake (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
