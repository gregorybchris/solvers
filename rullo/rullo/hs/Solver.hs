module Solver (solve, solveLine)
where

import Data.List

import Board

-- | Expand a tile into On/Off if Unknown or just a singleton list if On/Off
expandTile :: DataTile -> [DataTile]
expandTile t = let
  v = getDataValue t
  s = getDataState t
  in case s of
    Unknown -> [newDataTile v On, newDataTile v Off]
    _ -> [newDataTile v s]

-- | Expand a line into a list of possible lines
expandLine :: [DataTile] -> [[DataTile]]
expandLine [] = []
expandLine (t : []) = map (\t -> [t]) $ expandTile t
expandLine (t : ts) = [x : y | x <- expandTile t, y <- expandLine ts]

-- | Add up all tiles with state On
sumLine :: [DataTile] -> Int
sumLine [] = 0
sumLine (t : ts) = case getDataState t of
  On -> getDataValue t + sumLine ts
  _ -> sumLine ts

-- | Filter out all lines that don't sum to the goal
filterOptions :: [[DataTile]] -> Int -> [[DataTile]]
filterOptions tss g = filter (\b -> sumLine b == g) tss

-- | Collapse a tile's state based on an already seen state from the same line
collapseTile :: DataTile -> DataState -> DataTile
collapseTile t os = let
  v = getDataValue t
  s = getDataState t
  in case s == os of
    True -> newDataTile v s
    False -> newDataTile v Unknown

-- | Collapse a line to a single tile
collapseLine :: [DataTile] -> DataTile
collapseLine [] = error "ERROR: collapseLine running on empty line"
collapseLine (t : []) = t
collapseLine (ta : tb : ts) = collapseLine $ (collapseTile tb s) : ts
  where s = getDataState ta

-- | Solve a line
solveLine :: [DataTile] -> Int -> [DataTile]
solveLine ts g = map collapseLine $ transpose $ filterOptions (expandLine ts) g

data Line = Row Int | Col Int deriving (Show, Eq)

getBoardLines :: Board -> [Line]
getBoardLines b = rows ++ cols
  where rows = [Row r | r <- [0 .. getNRows b - 1]]
        cols = [Col c | c <- [0 .. getNCols b - 1]]

updateWithLine :: Board -> Line -> (Board, Bool)
updateWithLine b l = case l of
  Row r -> (updatedBoard, newLine /= line)
    where line = (getRow b r)
          newLine = solveLine line (getGoalValue (getRowGoal b r))
          updatedBoard = setRow b newLine r
  Col c -> (updatedBoard, newLine /= line)
    where line = (getCol b c)
          newLine = solveLine line (getGoalValue (getColGoal b c))
          updatedBoard = setCol b newLine c

updateByRowsCols :: Board -> [Line] -> Bool -> (Board, Bool)
updateByRowsCols b [] u = (b, u)
updateByRowsCols b (l : ls) u = let
  (updatedBoard, updated) = updateWithLine b l
  in case updated of
    True -> updateByRowsCols updatedBoard ls True
    False -> updateByRowsCols updatedBoard ls u

-- | Check if a Board is completely solved
checkSolved :: Board -> Bool
checkSolved b = and $ map (\t -> getDataState t /= Unknown) (concat $ getAllData b)

-- | Solve a Board
solve :: Board -> (Board, Bool)
solve b = let
  lines = getBoardLines b
  (updatedBoard, updated) = updateByRowsCols b lines False
  in case updated of
    False -> (updatedBoard, checkSolved updatedBoard)
    True -> solve updatedBoard
