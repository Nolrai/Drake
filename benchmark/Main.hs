module Main (main) where

import Drake (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
