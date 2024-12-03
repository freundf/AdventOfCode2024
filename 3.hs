module Main where

import Util
import Text.Regex.TDFA
import Data.Maybe (mapMaybe)

mul_regex :: String
mul_regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

do_regex :: String
do_regex = "do\\(\\)"

dont_regex :: String
dont_regex = "don\\'t\\(\\)"

eval_mul :: [String] -> Int
eval_mul [_, num1, num2] = read num1 * read num2 

part1 i = sum $ map eval_mul ((i =~ mul_regex) :: [[String]])

part2 i = 0



main :: IO ()
main = do
    input <- readInput 3
    printSolution (part1 input) (part2 input)

