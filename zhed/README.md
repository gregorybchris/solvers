# Zhed Solver

This is a solver for the puzzle game [Zhed](https://itunes.apple.com/us/app/zhed/id1209524498?mt=8).

It brute-forces all possible move combinations to solve simple levels.

Next Steps:
  - Include unit tests with [HUnit](https://hackage.haskell.org/package/HUnit)
  - Add pretty board printing
  - Check that formatting is correct
  - Introduce Maybe monad for error handling
  - Prune impossible branches from search
  - Make solver concurrent
  - Attempt to solve by backtracking
