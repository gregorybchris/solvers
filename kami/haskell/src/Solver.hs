module Solver (
  solve)
where

import Graph
import Puzzle

import qualified Data.List as List
import qualified Data.Maybe as Maybe

solve :: Puzzle -> Maybe [Contraction]
solve p = case getMoves p of
    -1 -> Nothing -- Ran out of moves
    nMoves -> let
      g = getGraph p
      in case getActiveSize g of
        1 -> Just $ reverse $ getContractions g -- Found a solution
        _ -> let
          contractions = getPossibleContractions g
          -- sortedContractions = List.sortOn (\(t, _) -> getNodeCentrality g t) contractions
          contractedGraphs = map (contract g) $ contractions
          updatedPuzzles = map (updateMovesAndGraph p $ nMoves - 1) contractedGraphs
          solutions = Maybe.catMaybes $ map solve updatedPuzzles
          in case solutions of
            [] -> Nothing
            firstSolution : _ -> Just firstSolution
