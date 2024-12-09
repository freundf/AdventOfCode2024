module Main where

import Util
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.List (sortBy, delete, insertBy)
import Data.Maybe (fromJust)


-- File functions

type FileSystem = ([File], [FreeBlock])
type FreeBlock = (Int, Int) -- (start, size)
type File = (Int, Int, Int) -- (id, start, size)

part1 i = 0
part2 = checksum . compressNoFragmentation . parseFileSystem

-- Parse
parseFileSystem :: [Char] -> FileSystem
parseFileSystem xs = parseAcc xs ([], []) 0 0
    where
        parseAcc [] (files, frees) _ _ = (reverse (sortBy (comparing getId) files), sortBy (comparing fst) frees)
        parseAcc [x] (files, frees) idx len = parseAcc [] (parseFile idx len (digitToInt x):files, frees) idx len
        parseAcc (x:y:xs) (files, frees) idx len = parseAcc xs (parseFile idx len x' : files, parseFree (len + x') y' : frees) (idx + 1) (len + y' + x')
            where
                x' = digitToInt x
                y' = digitToInt y

parseFile :: Int -> Int -> Int -> File
parseFile = (,,)

parseFree :: Int -> Int -> FreeBlock
parseFree = (,)


-- FileSystem

firstFree :: Int -> Int -> [FreeBlock] -> Maybe FreeBlock
firstFree size before []       = Nothing
firstFree size before (x:xs)
    | size <= (snd x) && before > (fst x)   = Just x
    | otherwise                             = firstFree size before xs

compressNoFragmentation :: FileSystem -> FileSystem
compressNoFragmentation fs = compressAcc fs []
    where
        compressAcc ([], frees) acc     = (acc, frees)
        compressAcc ((f:fs), frees) acc = case free of
            Nothing -> compressAcc (fs, frees) (f:acc)
            Just x  -> compressAcc (fs, newFrees) (newFile:acc)
            where
                x = fromJust free
                free = firstFree (getSize f) (getStart f) frees 
                newFrees
                    | snd x == (getSize f)  = mergeFree $ insertSort (getStart f, getSize f) $ delete x frees
                    | otherwise             = mergeFree $ insertSort (getStart f, getSize f) $ insertSort (fst x + (getSize f), snd x - (getSize f)) $ delete x frees
                    where
                        insertSort = insertBy (comparing fst)
                
                newFile = (getId f, fst x, getSize f)

mergeFree :: [FreeBlock] -> [FreeBlock]
mergeFree [x]                   = [x]
mergeFree (x:y:xs)
    | fst y == fst x + snd x    = mergeFree ((fst x, snd x + snd y) : xs)
    | otherwise                 = x : mergeFree (y:xs)

sortFS :: FileSystem -> FileSystem
sortFS (f, g) = (sortBy (comparing (\(x,y,z) -> y)) f, g)

checksum :: FileSystem -> Int
checksum = sum . map fileVal . fst
    where
        fileVal (id, start, size) = id * size * (2 * start + size - 1) `div` 2


-- File
getSize :: File -> Int
getSize (id, start, size) = size

getId :: File -> Int
getId (id, start, size) = id

getStart :: File -> Int
getStart (id, start, size) = start


example = "2333133121414131402"

main :: IO ()
main = do
    input <- readLines 9
    let line = head input
    printSolution (part1 example) (part2 line)
