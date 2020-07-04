module TestUtilities (runTest)
where

runTest :: Bool -> IO ()
runTest c
  | c = putStr "."
  | otherwise = error $ "ERROR"
