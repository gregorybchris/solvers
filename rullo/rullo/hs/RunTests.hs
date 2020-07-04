import qualified TestBoard as TestBoard (main)
import qualified TestSolver as TestSolver (main)

main :: IO ()
main = do
  TestBoard.main
  TestSolver.main
