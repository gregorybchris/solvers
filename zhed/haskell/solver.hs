{-|
Module      : Solver
Description : Uses brute force to find a list of moves that solve the level
Copyright   : Chris Gregory, 2017
License     : GPL-3

The Solver module brute forces all possible moves in order to find a solution
-}
module Solver (solve, Solution) where
import Game
import Board

import Debug.Trace

-- | A solution to the level
type Solution = Maybe [Move]

-- | Takes a board and produces a solution
solve :: Board -> Solution
solve b = let
  g = Game.newGame b
  in case (solveGameRec g) of
    Nothing -> Nothing
    Just moves -> Just (reverse moves)

-- | Takes a game and produces a solution
solveGameRec :: Game -> Solution
solveGameRec g = let
  options = Game.getGameOptions g
  moves = optionsToMoves options
  in tryAllMovesRec g moves

-- | Recursively tries moving every tile in every direction
tryAllMovesRec :: Game -> [Move] -> Solution
-- For debugging
-- tryAllMovesRec g _ | trace ("Solving: " ++ show g) False = undefined
tryAllMovesRec g [] = Nothing
tryAllMovesRec g (m:ms) = let
  newGame = Game.makeMove g m
  hasWon = Game.hasWon newGame
  in case hasWon of
    True -> Just (Game.getMoves newGame)
    False -> let
      solution = solveGameRec newGame
      in case solution of
        Nothing -> tryAllMovesRec g ms
        Just moves -> Just moves

-- | Turns all tile options into lists of moves that include directions
--
-- > In: [(1,2),(3,4)]
-- > Out: [[((1,2),U),((1,2),D),((1,2),L),((1,2),R)],((3,4),U),((3,4),D),((3,4),L),((3,4),R)]
--
optionsToMoves :: [Loc] -> [Move]
optionsToMoves locs = let
  allDirections = [(U :: Dir) ..]
  directionLists = map (\loc -> map (\d -> (loc, d)) allDirections) locs
  flattened = concat directionLists
  in flattened
