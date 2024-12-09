module Main where

import Util
import Data.Char (digitToInt)


-- File functions
type ID = Int
type Size = Int
type File = (ID, Size)

free :: ID
free = -1

expandFile :: File -> [File]
expandFile file@(id, size)
    | size == 1     = [file]
    | otherwise     = take size (repeat (id, 1))

expandFS :: [File] -> [File]
expandFS = concat . map expandFile

getSize :: File -> Int
getSize = snd

getId :: File -> Int
getId = fst

resize :: Int -> File -> File
resize x (id, size) = (id, x)




-- Util

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys)    = x:y:interleave xs ys
interleave _ _              = []



-- FS operations

compress :: ([File] -> [File]) -> [File] -> [File]
compress strategy xs
    | fst (firstFree xs) == -1  = xs
    | otherwise                 = compress strategy $ strip $ strategy xs


checksum :: [File] -> Int
checksum = foldl buildSum 0 . zip [0..]
    where
        buildSum acc (faktor, (id, _))
            | id == free    = acc
            | otherwise     = acc + (faktor * id)

fragment :: [File] -> [File]
fragment xs
    | (getSize freeFile) > (getSize back)     = take idx xs ++ [back] ++ [resize (getSize back) freeFile] ++ (init $ drop (idx + 1) xs)
    | (getSize freeFile) == (getSize back)    = take idx xs ++ [back] ++ (init $ drop (idx + 1) xs)
    | otherwise                         = take idx xs ++ [(getId back, getSize freeFile)] ++ (init $ drop (idx + 1) xs) ++ [resize (getSize freeFile) back]
    where 
        (idx, freeFile) = firstFree xs
        freeSize = getSize freeFile
        back = last xs

strip :: [File] -> [File]
strip xs
    | getId (last xs) == free  = init xs
    | otherwise             = xs

firstFree :: [File] -> (Int, File)
firstFree xs = firstFreeAcc xs 0
    where
        firstFreeAcc [] _ = (-1, (-1, -1))
        firstFreeAcc (x:xs) acc
            | fst x == free = (acc, x)
            | otherwise     = firstFreeAcc xs (acc + 1)

part1  = checksum . expandFS . compress fragment . parseInput

part2 i = 0


findFirstFitting :: Size -> [File] -> (Int, File)
findFirstFitting size xs = fittingAcc size xs 0
    where
        fittingAcc _ [] _ = (-1, (-1, -1))
        fittingAcc size (x:xs) acc
            | getId x == free && getSize x >= size  = (acc, x)
            | otherwise                             = fittingAcc size xs (acc + 1)



parseInput :: [Char] -> [File]
parseInput = zip (interleave [0..] $ repeat (-1)) . map digitToInt


example = "2333133121414131402"

main :: IO ()
main = do
    input <- readLines 9
    let line = head input
    printSolution (part1 example) (part2 example)
