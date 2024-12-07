module Main where

import Prelude hiding ((||))
import Util
import Control.Monad (replicateM)
import Data.List.Split (splitOn)


(||) :: (Integer -> Integer -> Integer)
y || x = read (show x ++ show y)

operations :: [Integer -> Integer -> Integer]
operations = [(+), (*)]



part1 = sum . map fst . filter (possible [(+), (*)]) . parseInput

part2 = sum . map fst . filter (possible [(+), (*), (||)]) . parseInput


possible :: [Integer -> Integer -> Integer] -> (Integer, [Integer]) -> Bool
possible ops (res, vals) = any (== res) $ map (evalExpression vals) opPermutations
    where 
        n = length vals - 1 
        opPermutations = replicateM n ops

equals :: Integer -> [Integer] -> [Integer -> Integer -> Integer] -> Bool
equals res (x:xs) ops = equalsAcc res (combine ops xs) x
    where 
        equalsAcc :: Integer -> [Integer -> Integer] -> Integer -> Bool
        equalsAcc res [] acc = res == acc
        equalsAcc res (f:fs) acc
            | acc > res         = False
            | otherwise         = equalsAcc res fs (f acc)

combine :: [(a -> a -> a)] -> [a] -> [(a -> a)]
combine = zipWith ($)

evalExpression :: [Integer] -> [(Integer -> Integer -> Integer)] -> Integer
evalExpression (x:xs) ops = foldl (flip ($)) x $ combine ops xs


parseInput :: [String] -> [(Integer, [Integer])]
parseInput = map parseLine
    where
        parseLine l = (read (head line), map read $ splitOn " " $ head (tail line))
            where line = splitOn ": " l


main = do
    input <- readLines 7
    printSolution (part1 input) (part2 input)
