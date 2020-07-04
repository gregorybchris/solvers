module TestSolver (main)
where

import Board
import Solver
import TestUtilities

smallBoard = let
  nr = 2
  nc = 3
  dvs = [[4, 6, 1],
         [1, 9, 3]]
  rgvs = [5, 12]
  cgvs = [4, 9, 4]
  in newBoard nr nc dvs rgvs cgvs

mediumBoard = let
  nr = 5
  nc = 5
  dvs = [[2, 8, 5, 3, 3],
         [1, 2, 5, 3, 3],
         [5, 3, 3, 7, 3],
         [4, 1, 1, 9, 5],
         [4, 9, 2, 5, 4]]
  rgvs = [18, 6, 10, 11, 15]
  cgvs = [10, 18, 11, 13, 8]
  in newBoard nr nc dvs rgvs cgvs

largeBoard = let
  nr = 6
  nc = 7
  dvs = [[ 1, 16, 12, 17,  1,  4, 17],
         [ 9, 14,  5,  5,  2,  1,  2],
         [ 4, 14,  2,  2,  6, 10, 17],
         [ 1,  1, 11, 12, 10,  7,  7],
         [14,  6,  3,  9, 12,  5, 14],
         [12,  9, 18, 10,  9, 16,  6]]
  rgvs = [18, 19, 31, 36, 37, 64]
  cgvs = [26, 15, 35, 38, 28, 17, 46]
  in newBoard nr nc dvs rgvs cgvs


testLineSolver :: IO ()
testLineSolver = do
  let line = map (\v -> newDataTile v Unknown) [2, 8, 5, 3, 3]
  let solvedLine = solveLine line 18
  let solvedValues = map (\t -> getDataValue t) solvedLine
  let solvedStates = map (\t -> getDataState t) solvedLine
  runTest $ solvedValues == [2, 8, 5, 3, 3]
  runTest $ solvedStates == [On, On, On, Unknown, Unknown]

testSolver :: IO ()
testSolver = do
  let expectedStates = [[On , On , On , On , Off],
                        [Off, Off, Off, On , On ],
                        [Off, Off, On , On , Off],
                        [On , On , On , Off, On ],
                        [On , On , On , Off, Off]]
  let (solvedBoard, solved) = solve mediumBoard
  let actualStates = map getDataState $ concat $ getAllData solvedBoard
  runTest $ solved == True
  runTest $ actualStates == concat expectedStates

main :: IO ()
main = do
  testLineSolver
  testSolver
