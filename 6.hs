module Main where

import Util
import Data.List (elem, elemIndex)
import Data.Maybe (fromJust)

type Position = (Int, Int)
type Direction = (Int, Int)
type Grid = [[Char]]
data Fail

directions :: [Direction]
directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

guard :: Char
guard = '^'

obstacle :: Char
obstacle = '#'

visited :: Char
visited = 'X'

simulate :: Grid -> Grid
simulate g = move initPos initDir g
    where
        move :: Position -> Direction -> Grid -> Grid
        move p@(x, y) d@(dx, dy) g
            | nextX < 0 || nextY < 0 || nextX >= sizeX || nextY >= sizeY    = g
            | get (nextX, nextY) g == obstacle                              = move p (nextDirection d) g
            | otherwise                                                     = move (nextX, nextY) d nextGrid
            where
                nextX = x + dx
                nextY = y + dy
                nextGrid = set (nextX, nextY) guard $ set p visited g
                nextDirection x = directions !! (((fromJust $ elemIndex x directions) + 1) `mod` (length directions)) 

        initDir = directions !! 0
        initPos = indexOf '^' g
        sizeX = length (head g)
        sizeY = length g


simulateMax :: Int -> Grid -> Either Grid Bool
simulateMax max g = move initPos initDir max 0 g
    where
        move :: Position -> Direction -> Int -> Int -> Grid -> Either Grid Bool
        move p@(x, y) d@(dx, dy) max steps g
            | steps > max                                                   = Right False
            | nextX < 0 || nextY < 0 || nextX >= sizeX || nextY >= sizeY    = Left g
            | get (nextX, nextY) g == obstacle                               = move p (nextDirection d) max steps g
            | otherwise                                                     = move (nextX, nextY) d max (steps + 1) nextGrid
            where
                nextX = x + dx
                nextY = y + dy
                nextGrid = set (nextX, nextY) guard $ set p visited g
                nextDirection x = directions !! (((fromJust $ elemIndex x directions) + 1) `mod` (length directions)) 

        initDir = directions !! 0
        initPos = indexOf '^' g
        sizeX = length (head g)
        sizeY = length g



indexOf :: Char -> Grid -> Position
indexOf c g = indexCounter c g 0
    where
        indexCounter _ [] _ = error ("Couldn't find: " ++ [c])
        indexCounter c (r:rs) counter
            | c `elem` r    = (fromJust $ elemIndex c r, counter)
            | otherwise     = indexCounter c rs (counter + 1)

get :: Position -> Grid -> Char
get (x, y) g = g !! y !! x

set :: Position -> Char -> Grid -> Grid
set (x, y) t g = take y g ++ [take x row ++ [t] ++ drop (x + 1) row] ++ drop (y + 1) g
    where row = g !! y

countVisited :: Grid -> Int
countVisited = length . filter (`elem` [guard, visited]) . concat

part1 :: Grid -> Int
part1 = countVisited . simulate


part2 i size = length $ filter fails $ variations2D $ resetGrid (indexOf '^' i) $ simulate i
    where fails x = either (\x -> False) not $ simulateMax size x 

resetGrid :: Position -> Grid -> Grid
resetGrid initPos grid = set initPos '^' $ set (indexOf '^' grid) 'X' grid

    
variations2D :: [[Char]] -> [[[Char]]]
variations2D grid = [set (i, j) '#' grid | i <- [0..length grid - 1], j <- [0..length (grid !! i) - 1], get (i, j) grid == visited]


main :: IO ()
main = do
    input <- readLines 6
    let size = (length input) * (length $ head input)
    printSolution (part1 input) (part2 input size)
