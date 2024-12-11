module Main where

import Util
import Data.List.Split (splitOn)





blink :: Int -> Int -> Int
blink count stone 
    | count == 0    = 1
    | stone == 0    = blink (count - 1) 1
    | even digits   = blink (count - 1) (fst splitStone) + blink (count - 1) (snd splitStone)
    | otherwise     = blink (count - 1) (2024 * stone)
    where
      digits = digitCount stone
      splitStone = splitHalf stone

digitCount :: Int -> Int
digitCount = (+1) . floor . logBase 10 . realToFrac

splitHalf :: Int -> (Int, Int)
splitHalf i = (i `div` (divisor), i `mod` (divisor))
    where
        divisor = 10 ^ halfLen 
        halfLen = (digitCount i) `div` 2

parseStones :: [[Char]] -> [Int]
parseStones = map read . splitOn " " . head

part1 = sum . map (blink 25) . parseStones

part2 i = 0

main = do
    input <- readLines 11
    printSolution (part1 input) (part2 input)
