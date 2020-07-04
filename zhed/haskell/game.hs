{-|
Module      : Game
Description : Abstracts away game operations like making moves
              and checking if the game has been won
Copyright   : Chris Gregory, 2017
License     : GPL-3

The Game module can make moves and check to see if the game has been won
-}
module Game (Game, Move, Dir(U, D, L, R), newGame, getGameOptions,
  hasWon, getMoves, makeMove) where
import Board

-- | A direction of a move (up, down, left, right)
data Dir
  = U -- ^ Up
  | D -- ^ Down
  | L -- ^ Left
  | R -- ^ Right
    deriving (Show, Enum)

-- | A possible move made up of a tile location and a direction
type Move = (Loc, Dir)

-- | The Game constructor. Maintains a list of moves, a board, and
--   whether the game has been won yet
newtype Game = Game ([Move], Board, Bool) deriving (Show)

-- | Creates a new game given a board
newGame :: Board -> Game
newGame b = Game ([], b, False)

-- | Returns a list of locations for all number tiles
getGameOptions :: Game -> [Loc]
getGameOptions (Game (moves, b, won)) = Board.getBoardOptions b

-- | Returns True when the goal tile has been reached by one of the moves
--   in the moves list
hasWon :: Game -> Bool
hasWon (Game (moves, b, won)) = won

-- | Returns the list of the moves that have been played in the game
getMoves :: Game -> [Move]
getMoves (Game (moves, b, won)) = moves

-- | Makes a move by updating the game board, the list of moves, and
--   whether the game has been won.
makeMove :: Game -> Move -> Game
makeMove (Game (moves, b, won)) ((r, c), d) = let
  loc = (r, c)
  initialTile = Board.get b loc
  in case initialTile of
    Number tileValue -> let
      move = (loc, d)
      initialNewBoard = Board.set b loc Board.Blank
      (newBoard, newWon) = updateBoardRec initialNewBoard move tileValue won
      in (Game (move:moves, newBoard, newWon))
    _ -> error "ERROR: Move must be a valid number tile"

-- | Recursively updates the board given a move and
updateBoardRec :: Board -- ^ The board to update
               -> Move -- ^ The move to perform
               -> Int -- ^ The value of the tile to move
               -> Bool -- ^ The previous hasWon value
               -> (Board, Bool) -- ^ The board and whether the game was won
updateBoardRec b ((r, c), d) tileValue hasWon
  | tileValue == 0 = (b, hasWon)
  | otherwise = let
      loc = (r, c)
      nextLoc = translateLocation loc d 1
      nextLocInBounds = Board.inBounds b nextLoc
      newMove = (nextLoc, d)
      in case nextLocInBounds of
        False -> (b, hasWon)
        True -> let
          transTile = Board.get b nextLoc
          in case transTile of
            Board.Empty -> let
              newBoard = Board.set b nextLoc Board.Blank
              in updateBoardRec newBoard newMove (tileValue - 1) hasWon
            Board.Goal -> updateBoardRec b newMove tileValue True
            _ -> updateBoardRec b newMove tileValue hasWon

-- | Takes a location, a direction, and an offset, and returns a location
--   that is offset by that amount in that direction
translateLocation :: Loc -> Dir -> Int -> Loc
translateLocation (r, c) d offset = case d of
  U -> (r + (-1 * offset), c)
  D -> (r + offset, c)
  L -> (r, c + (-1 * offset))
  R -> (r, c + offset)
