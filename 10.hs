module Main where

import Util
import Data.Char (digitToInt)
import Data.Array
import Data.Graph
import Data.List (transpose)



type Node = (Int, (Int, Int), [(Int, Int)])

value :: Node -> Int
value (val, idx, ns) = val 

top :: Int
top = 9


trailheads :: (Graph, Vertex -> Node) -> [Vertex]
trailheads (graph, nodeFromVertex) = [v | v <- vertices graph, value (nodeFromVertex v) == 0]

hills :: (Vertex -> Node) -> [Vertex] -> [Vertex]
hills nodeFromVertex = filter ((== top) . value . nodeFromVertex)

countTrails :: (Graph, Vertex -> Node) -> Vertex -> Int
countTrails (graph, nodeFromVertex) = length . hills nodeFromVertex . reachable graph
    

countUniqueTrails :: (Graph, Vertex -> Node) -> Vertex -> Int
countUniqueTrails g@(graph, nodeFromVertex) v
    | value (nodeFromVertex v) == top   = 1
    | otherwise                         = sum [countUniqueTrails g n | n <- (graph ! v)]

part1 i = sum $ map (countTrails g) $ trailheads g
    where g = parseGraph i

part2 i = sum $ map (countUniqueTrails g) $ trailheads g
    where g = parseGraph i


parseGraph :: [[Char]] -> (Graph, Vertex -> Node)
parseGraph g = (graph, nodeFromVertex)
    where
        grid = map (map digitToInt) g
        rows = length grid
        cols = length $ head grid

        asArray = listArray ((0, 0), (rows - 1, cols - 1)) $ concat grid
        neighbors (i, j) = [(x, y) | (x, y) <- [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)], x >= 0, x < rows, y >= 0, y < cols]
        
        idxToArray (i, j) = j * rows + i 


        edges (i, j) = [(x, y) | (x, y) <- neighbors (i, j), (asArray ! (x, y)) - (asArray ! (i, j)) == 1]    
  
        (graph, nodeFromVertex, _) = graphFromEdges [(asArray ! (i, j), (i, j), edges (i, j)) | i <- [0..rows - 1], j <- [0..cols - 1]]

main = do
    input <- readLines 10
    printSolution (part1 input) (part2 input)
