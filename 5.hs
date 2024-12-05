module Main where

import Util
import Data.List (sortBy, groupBy, intersect)
import Data.List.Split (splitOn)

type Rule = (Int, [Int])
type Update = [Int]


part1 :: [String] -> [String] -> Int
part1 r u = sum $ map middle $ filter (isOrdered rules) updates
    where
        rules = parseRules r
        updates = parseUpdates u    

part2 r u = 0

isOrdered :: [(Int, [Int])] -> [Int] -> Bool
isOrdered r xs = isOrderedAcc (filterPred xs r) xs True
    where
        isOrderedAcc :: [(Int, [Int])] -> [Int] -> Bool -> Bool 
        isOrderedAcc _ _ False  = False
        isOrderedAcc _ [] acc   = acc    
        isOrderedAcc r (x:xs) acc = isOrderedAcc (filterPred xs r) xs (acc && (getPred x r == []))


getPred :: Int -> [(Int, [Int])] -> [Int]
getPred i = concat . map snd . filter ((== i) . fst)

filterPred :: [Int] -> [(Int, [Int])] -> [(Int, [Int])]
filterPred l = map filterList . filter (\(x, _) -> elem x l)
    where filterList (y, ys) = (y, intersect l ys)


parseRules :: [String] -> [Rule]
parseRules = genPred . map parseRule
    where
        parseRule = toTuple . map read . splitOn "|"
            where toTuple [x, y] = (x, y)

        genPred = map combine . group' . sort'
            where
                group' = groupBy (\(_, x) (_, y) -> x == y)
                sort' = sortBy (\(_, x) (_, y) -> compare x y)
                combine x = (snd $ head x, map fst x)

       



parseUpdates :: [String] -> [Update]
parseUpdates = map parseUpdate
    where parseUpdate = map read . splitOn ","

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
    input <- readLines 5
    let [rules, updates] = splitOn [""] input
    printSolution (part1 rules updates) (part2 rules updates)

