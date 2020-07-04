{-|
Module      : Levels
Description : A collection of levels
Copyright   : Chris Gregory, 2017
License     : GPL-3

The Levels module has boards for several test levels
-}
module Levels (level6, level7, level12, level40) where

import Board

-- | Level 6 (4 Number Tiles)
--
-- > . . . . . . . .
-- > . . @ . . . 3 .
-- > . . . . . . . .
-- > . . . . . . . .
-- > . . 2 . . . . .
-- > . . . . . 3 . .
-- > . . . 2 . . . .
-- > . . . . . . . .
--
level6 :: Board
level6 = let
  bi = newBoard 8 8
  b1 = Board.set bi (1, 6) (Number 3)
  b2 = Board.set b1 (4, 2) (Number 2)
  b3 = Board.set b2 (6, 3) (Number 2)
  b4 = Board.set b3 (5, 5) (Number 3)
  bf = Board.set b4 (1, 2) Goal
  in bf


-- | Level 7 (5 Number Tiles)
--
-- > . . . . . . . .
-- > . . . . . . . .
-- > . . . 1 . . . .
-- > . @ . . . . 1 .
-- > . . . . . 1 . .
-- > . . 2 . 2 . . .
-- > . . . . . . . .
-- > . . . . . . . .
--
level7 :: Board
level7 = let
  bi = newBoard 8 8
  b1 = Board.set bi (2, 3) (Number 1)
  b2 = Board.set b1 (5, 2) (Number 2)
  b3 = Board.set b2 (5, 4) (Number 2)
  b4 = Board.set b3 (4, 5) (Number 1)
  b5 = Board.set b4 (3, 6) (Number 1)
  bf = Board.set b5 (3, 1) Goal
  in bf


-- | Level 12 (6 Number Tiles)
--
-- > . . . . . . . . .
-- > . . . . . . . . .
-- > . . 5 . . 4 . . .
-- > . 2 . . . . . . .
-- > . 1 . . . . . . .
-- > . . . 1 . . . . .
-- > . . . . . . . . .
-- > . 3 . . . . @ . .
-- > . . . . . . . . .
--
level12 :: Board
level12 = let
  bi = newBoard 9 9
  b1 = Board.set bi (2, 2) (Number 5)
  b2 = Board.set b1 (2, 5) (Number 4)
  b3 = Board.set b2 (3, 1) (Number 2)
  b4 = Board.set b3 (4, 1) (Number 1)
  b5 = Board.set b4 (5, 3) (Number 1)
  b6 = Board.set b5 (7, 1) (Number 3)
  bf = Board.set b6 (7, 6) Goal
  in bf


-- | Level 40 (7 Number Tiles)
--
-- > . . . . . . . . . .
-- > . . . . . . . . . .
-- > . . @ . . . . 2 . .
-- > . . . . . . . . . .
-- > . . . . . . . 1 . .
-- > . . . . . . . 3 . .
-- > . . . 2 3 . . . . .
-- > . . . . . 2 3 . . .
-- > . . . . . . . . . .
-- > . . . . . . . . . .
--
level40 :: Board
level40 = let
  bi = newBoard 10 10
  b1 = Board.set bi (2, 7) (Number 2)
  b2 = Board.set b1 (4, 7) (Number 1)
  b3 = Board.set b2 (5, 7) (Number 3)
  b4 = Board.set b3 (6, 3) (Number 2)
  b5 = Board.set b4 (6, 4) (Number 3)
  b6 = Board.set b5 (7, 5) (Number 2)
  b7 = Board.set b6 (7, 6) (Number 3)
  bf = Board.set b7 (2, 2) Goal
  in bf
