{-# LANGUAGE ForeignFunctionInterface #-}

module HsInterface where

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

import Board
import Solver

unflatten :: Num a => [a] -> Int -> [[a]]
unflatten a w
  | mod (length a) w /= 0 =
      error ("ERROR: unflatten failed with width " ++ (show w))
  | length a == w = [a]
  | otherwise = (take w a) : (unflatten (drop w a) w)

dataStateToIndicator :: DataState -> Int
dataStateToIndicator s = case s of
  On -> 1
  _ -> 0

getBoardIndicators :: Board -> Bool -> Int -> [Int]
getBoardIndicators board solved size = case solved of
  True -> map dataStateToIndicator resultStates
    where resultStates = map getDataState $ concat $ getAllData board
  False -> replicate size 0

solveHs :: Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr CInt)
solveHs dvsP rgvsP cgvsP nrP ncP = let
  nr = fromIntegral nrP :: Int
  nc = fromIntegral ncP :: Int
  in do flatDvsC <- peekArray (nr * nc) dvsP
        rgvsC <- peekArray nr rgvsP
        cgvsC <- peekArray nc cgvsP
        let dvs = unflatten (map fromIntegral flatDvsC) nc
        let rgvs = map fromIntegral rgvsC
        let cgvs = map fromIntegral cgvsC
        let inputBoard = newBoard nr nc dvs rgvs cgvs
        let (solvedBoard, solved) = solve inputBoard
        let indicators = getBoardIndicators solvedBoard solved (nr * nc)
        resultArray <- newArray (map fromIntegral indicators)
        return resultArray

foreign export ccall solveHs :: Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr CInt)
