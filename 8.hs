module Main where

import Util
import qualified Data.Map.Strict as Map
import Data.List (nub)


type Position = (Int, Int)

isAntenna :: Char -> Bool
isAntenna = (/= '.')

findAntennas :: [[Char]] -> Map.Map Char [Position]
findAntennas = foldr (Map.unionWith (++) . uncurry findAntennasLine) Map.empty . zip [0..]

findAntennasLine :: Int -> [Char] -> Map.Map Char [Position]
findAntennasLine n = foldl insertAntenna Map.empty . zip positions 
    where
        insertAntenna m (v, k) 
            | isAntenna k   = Map.insertWith (++) k [v] m
            | otherwise     = m

        positions = [ (n, p) | p <- [0..] ] 

antinodes :: (Int, Int) -> (Position, Position) -> [Position]
antinodes bounds (p, q) = filter (inBounds bounds) [p `pairPlus` v, q `pairMinus` v]
    where v = pairMinus p q

resonantAntinodes :: (Int, Int) -> (Position, Position) -> [Position]
resonantAntinodes bounds antennas = antinodeAcc bounds antennas [] 0
    where
        antinodeAcc :: (Int, Int) -> (Position, Position) -> [Position] -> Int -> [Position]
        antinodeAcc bounds (p, q) acc t
            | not (null nextAntinodes)  = antinodeAcc bounds (p, q) (nextAntinodes ++ acc) (t + 1)
            | otherwise                 = acc
            where
                nextAntinodes = filter (inBounds bounds) [nextP, nextQ]
                nextP = p `pairPlus` (t `pairTimes` v)
                nextQ = q `pairMinus` (t `pairTimes` v)
                v = pairMinus p q
            
pairTimes :: Num a => a -> (a, a) -> (a, a)
pairTimes t (x1, y1) = (t * x1, t * y1)

pairPlus :: Num a => (a, a) -> (a, a) -> (a, a)
pairPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pairMinus :: Num a => (a, a) -> (a, a) -> (a, a)
pairMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:xs) = (map (x,) xs) ++ (pairs xs)

size :: [[a]] -> (Int, Int)
size x = (length $ head x, length x)

inBounds :: (Int, Int) -> Position -> Bool
inBounds (maxX, maxY) (x, y) = x >= 0 && x < maxX && y >= 0 && y < maxY

countUnique :: Eq a => [[[a]]] -> Int
countUnique = length . nub . concat . concat

part1 :: [[Char]] -> Int
part1 i = countUnique $ map (map (antinodes (size i)) . pairs . snd) $ Map.toList $ findAntennas i

part2 :: [[Char]] -> Int
part2 i = countUnique $ map (map (resonantAntinodes (size i)) . pairs . snd) $ Map.toList $ findAntennas i

main :: IO ()
main = do
    input <- readLines 8
    printSolution (part1 input) (part2 input)
