module Main where

import Util
import Data.Array
import Data.Graph
import Data.Char (digitToInt)
import Data.Tree
import Data.Maybe (fromJust)


type Node = (Char, (Int, Int), [(Int, Int)])

key :: Node -> (Int, Int)
key (v, k, es) = k

val :: Node -> Char
val (v, k, es) = v

edges :: Node -> [(Int, Int)]
edges (v, k, es) = es


price :: (Tree Vertex -> Int) -> (Tree Vertex -> Int) -> Tree Vertex -> Int
price cost1 cost2 x = cost1 x * cost2 x

fullPrice :: (Tree Vertex -> Int) -> (Tree Vertex -> Int) -> Graph -> Int
fullPrice cost1 cost2 g = sum . map (price cost1 cost2) . components $ g

countCorners :: Int -> Graph -> Array Vertex Int -> Tree Vertex -> Int
countCorners dims graph outdegrees vertices = foldr (count dims graph) 0 $ vertices
    where
        count (rows, cols) graph x acc = acc + case outdegrees ! x of
            4 -> length . filter (not . path graph x) $ cornerNeighbors x
            3 ->  if pathX (x - cols - 1) then 0 else 1
                + if pathX (x - cols + 1) then 0 else 1
                + if pathX (


            2 -> if abs ((-) (head (graph ! x)) (last (graph ! x))) `elem` [2, 2 * cols] 
                    then 0 else 1
            1 -> 2
            0 -> 4
        where
            pathX = path graph x



?X?
XXX
X0

cornerNeighbors (rows, cols) x = [y | y <- [x - cols - 1, x - cols + 1, x + cols - 1, x + cols + 1], y >= 0, y <= rows * cols] 


perimeter :: Array Vertex Int -> Tree Vertex -> Int
perimeter outdegrees = foldr (\x acc -> acc + (4 - (outdegrees ! x))) 0

area :: Tree Vertex -> Int
area = foldr (\_ acc -> acc + 1) 0

parseGraph :: [[Char]] -> Graph
parseGraph g = graph
    where
        grid = gridToArray g
                
        rows = length g
        cols = length . head $ g      

        gridToArray grid = listArray ((0, 0), (rows - 1, cols - 1)) . concat $ grid

        neighbors (i, j) = [(x, y) | (x, y) <-  [(i, j - 1), (i, j + 1), (i - 1, j), (i + 1, j)], x >= 0, x < rows, y >= 0, y < cols]

        edges (i, j) = [(x, y) | (x, y) <- neighbors (i, j), (grid ! (x, y)) == (grid ! (i, j))]

        (graph, _, _) = graphFromEdges [(grid ! (i, j), (i, j), edges (i, j)) | i <- [0..rows - 1], j <- [0..cols - 1]] 

 
example = [ "RRRRIICCFF",
            "RRRRIICCCF",
            "VVRRRCCFFF",
            "VVRCCCJFFF",
            "VVVVCJJCFE",
            "VVIVCCJJEE",
            "VVIIICJJEE",
            "MIIIIIJJEE",
            "MIIISIJEEE",
            "MMMISSJEEE"]

part1 i = let graph = parseGraph i in fullPrice (perimeter (outdegree graph)) area $ graph

part2 i = let graph = parseGraph i in fullPrice (countCorners (length . head $ i) graph (outdegree graph)) area $ graph



main = do 
    input <- readLines 12
    printSolution (part1 input) (part2 example)
