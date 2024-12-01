module Util where

import System.IO

input_dir :: String
input_dir = "input/"


readInput i = readFile $ input_dir ++ show i

readLines i = do
    contents <- readInput i
    return (lines contents)

printSolution :: (Show a, Show b) => a -> b -> IO () 
printSolution s1 s2 = do
    putStrLn ("1. " ++ show s1)
    putStrLn ("2. " ++ show s2)
