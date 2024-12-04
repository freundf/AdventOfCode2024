module Main where

import Util
import Text.Regex.TDFA (getAllTextMatches , (=~))
import Data.List (transpose, tails)

xmasRegex :: String
xmasRegex = "XMAS"

checkLine :: String -> Int
checkLine l = length $ check l ++ check (reverse l)
    where check l = getAllTextMatches (l =~ xmasRegex) :: [String]

diagonals :: [[a]] -> [[a]]
diagonals []        = []
diagonals ([]:xss)  = xss
diagonals xss       = zipWith (++) (map ((:[]) . head) xss ++ repeat []) ([]:(diagonals (map tail xss)))

part1 :: [String] -> Int
part1 i = sum (map checkLine i) 
        + sum (map checkLine (transpose i)) 
        + sum (map checkLine (diagonals i)) 
        + sum (map checkLine (diagonals (map reverse i)))

part1' :: [String] -> Int
part1' = sum . map hasXmas . genSubGrid 7 . extend 7 '_'

extend :: Int -> a -> [[a]] -> [[a]]
extend n x grid = fillCols ++ map (\x -> fillRows ++ x ++ fillRows) grid ++ fillCols
    where 
        len = length (grid !! 0)
        fillRows = take n $ repeat x 
        fillCols = take n $ repeat $ take (n + len + n) $ repeat x    


replace :: Int -> Int -> a -> [[a]] -> [[a]]
replace row col val x = take row x ++ [replaceInRow (x !! row)] ++ drop (row + 1) x
    where replaceInRow r = take col r ++ [val] ++ drop (col + 1) r


hasXmas :: [[Char]] -> Int
hasXmas x@[[ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'X', 'M', 'A', 'S'],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ]]     = 1 + (hasXmas $ replace 3 4 '_' x)

hasXmas x@[[ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           ['S', 'A', 'M', 'X',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ]]     = 1 + (hasXmas $ replace 3 2 '_' x)

hasXmas x@[[ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'X',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'M',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'A',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'S',  _ ,  _ ,  _ ]]    = 1 + (hasXmas $ replace 4 3 '_' x)

hasXmas x@[[ _ ,  _ ,  _ , 'S',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'A',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'M',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'X',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ]]     = 1 + (hasXmas $ replace 2 3 '_' x)

hasXmas x@[[ _ ,  _ ,  _ ,  _ ,  _ ,  _ , 'S'],
           [ _ ,  _ ,  _ ,  _ ,  _ , 'A',  _ ],
           [ _ ,  _ ,  _ ,  _ , 'M',  _ ,  _ ],
           [ _ ,  _ ,  _ , 'X',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ]]     = 1 + (hasXmas $ replace 2 4 '_' x)

hasXmas x@[[ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'X',  _ ,  _ ,  _ ],
           [ _ ,  _ , 'M',  _ ,  _ ,  _ ,  _ ],
           [ _ , 'A',  _ ,  _ ,  _ ,  _ ,  _ ],
           ['S',  _ ,  _ ,  _ ,  _ ,  _ ,  _ ]]     = 1 + (hasXmas $ replace 4 2 '_' x)

hasXmas x@[[ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'X',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ , 'M',  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ , 'A',  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ , 'S']]     = 1 + (hasXmas $ replace 4 4 '_' x)

hasXmas x@[['S',  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ , 'A',  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ , 'M',  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ , 'X',  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ],
           [ _ ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ ]]     = 1 + (hasXmas $ replace 2 2 '_' x)

hasXmas _                                           = 0




part2 :: [String] -> Int
part2 = length . filter id . map isXmas . genSubGrid 3

isXmas :: [[Char]] -> Bool
isXmas [['M',  _ , 'S'],
        [ _ , 'A',  _ ],
        ['M',  _ , 'S']]    = True

isXmas [['M',  _ , 'M'],
        [ _ , 'A',  _ ],
        ['S',  _ , 'S']]    = True

isXmas [['S',  _ , 'M'],
        [ _ , 'A',  _ ],
        ['S',  _ , 'M']]    = True

isXmas [['S',  _ , 'S'],
        [ _ , 'A',  _ ],
        ['M',  _ , 'M']]    = True
       
isXmas _                    = False


windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((==n) . length) . map (take n) . tails

genSubGrid :: Int -> [[a]] -> [[[a]]]
genSubGrid n grid = concatMap (map transpose . windows n . transpose) (windows n grid)

main :: IO ()
main = do
    input <- readLines 4
    printSolution (part1' input) (part2 input)


