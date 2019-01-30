module Main where

import BuildGraph 
import System.Environment


main :: IO()
main = do
    [arg1] <- getArgs
    writeDot arg1
