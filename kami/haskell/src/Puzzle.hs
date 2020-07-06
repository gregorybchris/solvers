module Puzzle (
  Puzzle,
  newPuzzle,
  getName, getMoves, getGraph,
  updateMoves, updateGraph, updateMovesAndGraph)
where

import Graph

-- Name, moves, graph
newtype Puzzle = Puzzle (String, Int, Graph) deriving (Show)

newPuzzle :: String -> Int -> Graph -> Puzzle
newPuzzle n m g = Puzzle (n, m, g)

getName :: Puzzle -> String
getName (Puzzle (n, _, _)) = n

getMoves :: Puzzle -> Int
getMoves (Puzzle (_, m, _)) = m

updateMoves :: Puzzle -> Int -> Puzzle
updateMoves (Puzzle (n, _, g)) m = Puzzle (n, m, g)

getGraph :: Puzzle -> Graph
getGraph (Puzzle (_, _, g)) = g

updateGraph :: Puzzle -> Graph -> Puzzle
updateGraph (Puzzle (n, m, _)) g = Puzzle (n, m, g)

updateMovesAndGraph :: Puzzle -> Int -> Graph -> Puzzle
updateMovesAndGraph (Puzzle (n, _, _)) m g = Puzzle (n, m, g)
