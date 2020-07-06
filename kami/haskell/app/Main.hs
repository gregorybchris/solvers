module Main where

import Graph
import Loader
import Puzzle
import Solver

jsonFile :: FilePath
-- jsonFile = "puzzles/user-1.json"
jsonFile = "puzzles/kami-6-5.json"

main :: IO ()
main = do
  puzzle <- loadPuzzle jsonFile
  print puzzle
  let solution = solve puzzle
  putStrLn ""
  putStrLn "Solution"
  print solution
