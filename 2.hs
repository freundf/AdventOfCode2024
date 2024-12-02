module Main where

import Util
import Data.List
import Data.Ord


toInts :: String -> [Int]
toInts str = map read (words str)

isDecr :: [Int] -> Bool
isDecr x = sortBy (comparing Down) x == x

isIncr :: [Int] -> Bool
isIncr x = sort x == x

maxDiff3 :: [Int] -> Bool
maxDiff3 [x] = True
maxDiff3 (x:y:xs)
    | abs (x - y) > 3 || x == y     = False
    | otherwise                     = maxDiff3 (y:xs)
--    where 
--        maxDiffAcc _ False      = False
--        maxDiffAcc [x] acc      = acc
--        maxDiffAcc (x:y:xs) acc = maxDiffAcc (y:xs) (abs (x - y) <= 3 && acc

isSafe :: [Int] -> Bool
isSafe x = (isIncr x || isDecr x) && maxDiff3 x

part1 :: [String] -> Int
part1 i = length $ filter isSafe (map toInts i) 

part2 i = 0


main :: IO ()
main = do
    input <- readLines 2
    print $ filter isSafe (map toInts input)
    printSolution (part1 input) (part2 input)
