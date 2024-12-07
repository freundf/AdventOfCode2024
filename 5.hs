module Main where

import Util
import Data.List (sortBy, groupBy, intersect)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Function (on)

type Rule = (Int, [Int])
type Update = [Int]


part1 :: [Rule] -> [Update] -> Int
part1 rules updates = sum $ map middle $ filter (isOrdered rules) updates

part2 :: [Rule] -> [Update] -> Int
part2 rules updates = sum $ map middle $ map (orderUpdate rules) $ filter (not . isOrdered rules) updates

orderUpdate :: [Rule] -> Update -> Update
orderUpdate r u = sortBy (comparing predLen) u
    where 
        predLen z = length $ getPred z rules
        rules = filterRules u r

isOrdered :: [Rule] -> Update -> Bool 
isOrdered _ []              = True    
isOrdered r (x:xs)
    | getPred x rules == [] = isOrdered rules xs
    | otherwise             = False
    where
        rules = filterRules (x:xs) r

getPred :: Int -> [Rule] -> [Int]
getPred i = concat . map snd . filter ((== i) . fst)

filterRules :: Update -> [Rule] -> [Rule]
filterRules update = map isectSnd . filter elemFst
    where 
        isectSnd (y, ys) = (y, intersect update ys)
        elemFst (x, _) = elem x update

parseRules :: [String] -> [Rule]
parseRules = genPred . map parseRule
    where
        parseRule = tuple . map read . splitOn "|"
            where tuple [x, y] = (x, y)

        genPred = map combine . group' . sort'
            where
                group' = groupBy ((==) `on` snd)
                sort' = sortBy (comparing snd)
                combine x = (snd $ head x, map fst x)

parseUpdates :: [String] -> [Update]
parseUpdates = map parseUpdate
    where parseUpdate = map read . splitOn ","

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
    input <- readLines 5
    let (rules, updates) = (parseRules r, parseUpdates u) where [r, u] = splitOn [""] input
    printSolution (part1 rules updates) (part2 rules updates)

