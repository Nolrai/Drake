module Main (main) where

import Drake (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
