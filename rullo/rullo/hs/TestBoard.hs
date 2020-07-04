module TestBoard (main)
where

import Board
import TestUtilities

testBoardGetter :: IO ()
testBoardGetter = do
  let board = newBoard 2 3 [[4, 6, 1], [1, 9, 3]] [5, 12] [4, 9, 4]
  runTest $ (getGoalValue $ getRowGoal board 1) == 12
  runTest $ (getGoalValue $ getColGoal board 2) == 4
  runTest $ (getGoalState $ getRowGoal board 0) == Unsat

testDataTile :: IO ()
testDataTile = do
  let dataTile1 = newDataTile 4 On
  runTest $ (getDataValue dataTile1) == 4
  runTest $ (getDataState dataTile1) == On
  let dataTile2 = newDataTile (-1) Unknown
  runTest $ (getDataValue dataTile2) == -1
  runTest $ (getDataState dataTile2) == Unknown

testGoalTile :: IO ()
testGoalTile = do
  let goalTile = newGoalTile 5 Sat
  runTest $ (getGoalValue goalTile) == 5
  runTest $ (getGoalState goalTile) == Sat

testDataSetters :: IO ()
testDataSetters = do
  let board = newBoard 2 3 [[4, 6, 1], [1, 9, 3]] [5, 12] [4, 9, 4]
  let updatedBoard1 = setRow board (getRow board 0) 1
  runTest $ (getRow updatedBoard1 0) == (getRow updatedBoard1 1)
  let updatedBoard2 = setCol board (getCol board 0) 1
  runTest $ (getCol updatedBoard2 0) == (getCol updatedBoard2 1)

main :: IO ()
main = do
  testBoardGetter
  testDataTile
  testGoalTile
  testDataSetters
