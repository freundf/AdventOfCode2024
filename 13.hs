module Main where

import Util
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~), getAllTextMatches)
import Data.Either (fromRight)

data Matrix2x2 = Matrix2x2
    { a :: Double, b :: Double,
      c :: Double, d :: Double
    } deriving (Show, Eq)

type Vec2 = (Double, Double)

inverse :: Matrix2x2 -> Matrix2x2
inverse m = Matrix2x2 (d m / det) (- b m / det) (- c m / det) (a m / det)
    where det = determinant m

dot :: Matrix2x2 -> Vec2 -> Vec2
dot m (x, y) = (a m * x + b m * y, c m * x + d m * y)

determinant :: Matrix2x2 -> Double
determinant m = a m * d m - b m * c m

isSolution :: Int -> Vec2 -> Bool
isSolution max (x, y) = check x && check y
    where 
        check x = isInt x && round x <= max
        isInt x = abs (x - fromInteger (round x)) < 1e-3


solve :: Int -> (Matrix2x2, Vec2) -> Either Bool Int
solve max (m, v) = if isSolution max solution 
                then Right (3 * round (fst solution) + round (snd solution))
                else Left False
    where
        solution = inverse m `dot` v 

parseInput :: [String] -> [(Matrix2x2, Vec2)]
parseInput = map parseMachine . splitOn [""]
    where
        parseMachine :: [String] -> (Matrix2x2, Vec2)
        parseMachine [a, b, p] = (parseMatrix . map (parseLine buttonRegex) $ [a, b], parseLine prizeRegex p)
        
        parseMatrix [(a, c), (b, d)] = Matrix2x2 a b c d 

        parseLine :: String -> String -> Vec2
        parseLine regex i = (fromIntegral . read . head $ match, fromIntegral . read . head . tail $ match)
            where (_, _, _, match) = i =~ regex :: (String, String, String, [String])

        prizeRegex = "Prize: X=([0-9]+), Y=([0-9]+)"
        buttonRegex = "Button [AB]: X\\+([0-9]+), Y\\+([0-9]+)"
        

shiftPrice :: Double -> (Matrix2x2, Vec2) -> (Matrix2x2, Vec2)
shiftPrice x (m, (v1, v2)) = (m, (v1 + x, v2 + x)) 


part1 = sum . map (fromRight 0 . solve 100) . parseInput

part2 = sum . map (fromRight 0 . solve (maxBound :: Int)) . map (shiftPrice 10000000000000) . parseInput


main = do
    input <- readLines 13
    printSolution (part1 input) (part2 input)
