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

isSafe :: [Int] -> Bool
isSafe x = (isIncr x || isDecr x) && maxDiff3 x

part1 :: [String] -> Int
part1 i = length $ filter isSafe (map toInts i) 


removeEach :: [Int] -> [[Int]]
removeEach l = [take i l ++ drop (i + 1) l | i <- [0 .. length l - 1]]

anySafe :: [[Int]] -> Bool
anySafe l = any (== True) (map isSafe l)

part2 :: [String] -> Int
part2 i = length $ filter anySafe (map (removeEach . toInts) i)


main :: IO ()
main = do
    input <- readLines 2
    printSolution (part1 input) (part2 input)
