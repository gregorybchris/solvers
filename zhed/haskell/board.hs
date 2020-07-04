{-|
Module      : Board
Description : A square Zhed board with tiles
Copyright   : Chris Gregory, 2017
License     : GPL-3

The Board module can get and set tiles by location
and retrieve a list of all numbered tiles that a user
could manipulate.
-}
module Board (Board, Loc, Tile(Empty, Blank, Goal, Number),
  newBoard, get, set, width, height, inBounds, getBoardOptions)
where

-- | Location on board (row, col)
type Loc = (Int, Int)

-- | A Tile at one location on the board
data Tile
  = Empty -- ^ Empty tile
  | Blank -- ^ Tile that has been used or moved onto
  | Goal -- ^ The goal tile
  | Number Int -- ^ Any tile with a number on it
    deriving (Show)

-- | The Board constructor. Maintains width, height, and a 2D list of
--   all tiles on the board
newtype Board = Board (Int, Int, [[Tile]]) deriving (Show)

-- | Creates a new empty board given a width and height
newBoard :: Int -- ^ Width
         -> Int -- ^ Height
         -> Board -- ^ The new board
newBoard w h = Board (w, h, replicate h (replicate w Empty))

-- | Gets a tile from the board given a location
get :: Board -> Loc -> Tile
get (Board (w, h, ts)) (r, c)
  | not (inBounds (Board (w, h, ts)) (r, c)) =
      error ("ERROR: Board.get OOB @ " ++ (show (r, c)))
  | otherwise = ts !! r !! c

-- | Sets the board tile at a given location with a given tile type
set :: Board -> Loc -> Tile -> Board
set (Board (w, h, ts)) (r, c) t
  | not (inBounds (Board (w, h, ts)) (r, c)) =
      error ("ERROR: Board.set OOB @ " ++ (show (r, c)))
  | otherwise = let
      replace = (\xs e i -> take i xs ++ [e] ++ drop (i + 1) xs)
      nts = replace ts (replace (ts !! r) t c) r
      in (Board (w, h, nts))

-- | Gets the width of the board
width :: Board -> Int
width (Board (w, h, ts)) = w

-- | Gets the height of the board
height :: Board -> Int
height (Board (w, h, ts)) = h

-- | Returns True when the given location is within the board's bounds
inBounds :: Board -> Loc -> Bool
inBounds (Board (w, h, ts)) (r, c) = r >= 0 && r < h && c >= 0 && c < w

-- | Returns a list of locations for all number tiles
--
-- > . . . . . . . .
-- > . . @ . . . 3 .
-- > . . . . . . . .
-- > . . . . . . . .
-- > . . 2 . . . . .   ->   [(1,6),(4,2),(5,5),(6,3)]
-- > . . . . . 3 . .
-- > . . . 2 . . . .
-- > . . . . . . . .
--
getBoardOptions :: Board -> [Loc]
getBoardOptions (Board (w, h, ts)) = let
  flattened = concat ts
  enumerated = zip [0..] flattened
  indexToLocation = (\w h indexedTile -> let
    (index, value) = indexedTile
    in ((div index w, mod index h), value))
  withLocations = map (indexToLocation w h) enumerated
  isNumberTile = (\locTile -> case locTile of
      (_, (Number _)) -> True
      _ -> False)
  onlyNumberTiles = filter isNumberTile withLocations
  extractLocation = (\locTile -> let (loc, tile) = locTile in loc)
  onlyLocations = map extractLocation onlyNumberTiles
  in onlyLocations
