module Main where

import Util
import Data.List


processLine :: String -> (Int, Int)
processLine line = (read $ head i, read $ last i)
    where i = words line

splitTupleList :: [(Int, Int)] -> ([Int], [Int])
splitTupleList list = splitAcc list ([], [])
    where 
        splitAcc [] acc     = acc
        splitAcc (x:xs) acc = splitAcc xs ((fst x : fst acc), (snd x : snd acc))    

sortInTuple :: Ord a => ([a], [a]) -> ([a], [a])
sortInTuple (x, y) = (sort x, sort y)

getMinDiffs :: ([Int], [Int]) -> Int
getMinDiffs t = getMinDiffsAcc (sortInTuple t) 0
    where
        getMinDiffsAcc ([], []) acc         = acc
        getMinDiffsAcc ((x:xs), (y:ys)) acc = getMinDiffsAcc (xs, ys) (acc + abs (x - y))

part1 :: [String] -> Int
part1 input = getMinDiffs $ splitTupleList $ map processLine input

main :: IO ()
main = do
    input <- readLines 1
    printSolution (part1 input) 0
