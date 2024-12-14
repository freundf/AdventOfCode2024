module Main where

import Util
import Text.Regex.TDFA ((=~))
import Data.List (group, sort, sortBy, groupBy)
import Data.Char (intToDigit, digitToInt)
import Data.Array

type Vec2 = (Int, Int)
data Robot = Robot { p :: Vec2, v :: Vec2 }
    deriving (Eq, Show)

data Quadrant = NW | NE | SW | SE | None
    deriving (Eq, Show, Ord)


size = (101, 103)

bound :: Vec2 -> Vec2
bound (x, y) = (x `mod` fst size, y `mod` snd size)

move :: Int -> Robot -> Robot
move t r = Robot (bound $ p r `add` (t `times` v r)) (v r)

add :: Vec2 -> Vec2 -> Vec2
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

times :: Int -> Vec2 -> Vec2
times t (x, y) = (t * x, t * y) 

quadrants :: [Vec2] -> [Int]
quadrants = map length . group . sort . filter (not . (== None)) . map (toQuadrant)

midX = fst size `div` 2
midY = snd size `div` 2
toQuadrant (x, y)
            | x < midX && y < midY  = NW
            | x > midX && y < midY  = NE
            | x < midX && y > midY  = SW
            | x > midX && y > midY  = SE
            | otherwise             = None


parseInput :: [String] -> [Robot]
parseInput = map parseRobot
    where
        parseRobot line = let (_, _, _, [px, py, vx, vy]) = (line =~ robotRegex) :: (String, String, String, [String])
                            in Robot (read px, read py) (read vx, read vy)
        robotRegex = "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)"


part1 = product . quadrants . map (p . move 100) . parseInput

part2 i = map (printRobots robots) [0..10000]
    where 
        robots = parseInput i
        printRobots rs t = putStrLn . (drawRobots t) . map (move t) $ rs
    

drawRobots :: Int -> [Robot] -> String
drawRobots t = (\x -> "\n\n After " ++ show t ++ " seconds\n" ++ x) . unlines  . showTable . foldl insert grid . map p
    where
        arrayBounds = ((0, 0), (fst size - 1, snd size - 1))
        grid = array arrayBounds [((i, j), '.') | i <- [0..fst size - 1], j <- [0..snd size - 1]]

        insert grid pos = if grid ! pos == '.' 
                            then grid // [(pos, '1')] 
                            else grid // [(pos, intToDigit $ digitToInt (grid ! pos) + 1)]

showTable a = [[ a ! (i, j) | i <- [iMin..iMax]] |  j <- [jMin..jMax]]
      where
        ((iMin, jMin), (iMax, jMax)) = bounds a

main :: IO ()
main = do
    input <- readLines 14
    printSolution (part1 input) 0
    sequence_ (part2 input)
