module Main where

import Util
import Data.List (elem, elemIndex, transpose)
import Data.Maybe (fromJust)

data Direction = CW | CCW

guard :: Char
guard = '^'

obstacle :: Char
obstacle = '#'

visited :: Char
visited = 'X'

rotate :: Direction -> [[a]] -> [[a]]
rotate CW   = map reverse . transpose
rotate CCW  = reverse . transpose

move :: [[Char]] -> Int
move grid
    | elem guard (head grid)    = countVisited grid
    | otherwise                 = move $ nextMove grid

nextMove :: [[Char]] -> [[Char]]
nextMove grid = nextMoveHelper grid [] grid
    where
        nextMoveHelper (r1:r2:rs) acc copy 
            | elem guard r2     = if collision (r1, r2) then rotate CCW copy
                                    else acc ++ moveGuard (r1, r2) ++ rs
            | otherwise         = nextMoveHelper (r2:rs) (acc ++ [r1]) copy
        
collision :: (String, String) -> Bool
collision (x, y) = x !! idx == obstacle
    where idx = fromJust $ elemIndex guard y

moveGuard :: (String, String) -> [[Char]]
moveGuard (x, y) = [replace idx guard x, replace idx visited y]
    where idx = fromJust $ elemIndex guard y

countVisited :: [[Char]] -> Int
countVisited = length . filter isVisited . foldr (++) []
    where 
        isVisited x = x == guard || x == visited

replace :: Int -> a -> [a] -> [a]
replace i v xs = take i xs ++ [v] ++ drop (i + 1) xs


part1 = move

part2 i = 0


main = do
    input <- readLines 6
    printSolution (part1 input) (part2 input)

