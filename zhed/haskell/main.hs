{-|
Module      : Main
Description : Solves a Zhed level
Copyright   : Chris Gregory, 2017
License     : GPL-3

The Main module runs the solver on a level
-}
module Main where

import Solver
import Levels

-- | main function for the solver
main :: IO ()
main = do putStrLn (showSolution (Solver.solve Levels.level12))

-- | Converts a list of moves to a string
showSolution :: Solution -> String
showSolution sol = case sol of
  Nothing -> "No solution was found"
  Just moves -> "Solution: " ++ (show moves)
