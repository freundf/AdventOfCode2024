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

part2 r u = sum $ map middle $ map (orderUpdate rules) $ filter (\u -> not (isOrdered rules u)) updates
    where 
        rules = parseRules r
        updates = parseUpdates u

orderUpdate :: [Rule] -> Update -> Update
orderUpdate r u = sortBy (\x y -> compare (predLen x) (predLen y)) u
    where 
        predLen z = length $ getPred z rules
        rules = filterRules u r

isOrdered :: [Rule] -> Update -> Bool
isOrdered r xs = isOrderedAcc (filterRules xs r) xs True
    where
        isOrderedAcc :: [(Int, [Int])] -> [Int] -> Bool -> Bool 
        isOrderedAcc _ _ False  = False
        isOrderedAcc _ [] acc   = acc    
        isOrderedAcc r (x:xs) acc = isOrderedAcc (filterRules xs r) xs (acc && (getPred x r == []))


getPred :: Int -> [Rule] -> [Int]
getPred i = concat . map snd . filter ((== i) . fst)

filterRules :: Update -> [Rule] -> [Rule]
filterRules u = map filterList . filter (\(x, _) -> elem x u)
    where filterList (y, ys) = (y, intersect u ys)


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

