module Board (
  newDataTile, getDataValue, getDataState, DataTile, DataState(On, Off, Unknown),
  newGoalTile, getGoalValue, getGoalState, GoalTile, GoalState(Sat, Unsat),
  newBoard, getNRows, getNCols, inBounds, Board,
  getData, setData, getAllData, getRow, setRow, getCol, setCol,
  getRowGoal, getColGoal)
where

data DataState = On | Off | Unknown deriving (Show, Eq)
data GoalState = Sat | Unsat deriving (Show, Eq)

newtype DataTile = DataTile (Int, DataState) deriving (Show, Eq)
newtype GoalTile = GoalTile (Int, GoalState) deriving (Show, Eq)

newtype Board = Board (Int, Int, [[DataTile]], [GoalTile], [GoalTile]) deriving (Show)

-- | Construct a new Board
newBoard :: Int -- ^ Number of rows in Board
         -> Int -- ^ Number of columns in Board
         -> [[Int]] -- ^ Two-dimensional list of data values in the Board
         -> [Int] -- ^ List of goal values for the rows
         -> [Int] -- ^ List of goal values for the columns
         -> Board -- ^ New Board
newBoard nr nc dvs rgvs cgvs
  | nr == 0 || nc == 0 =
    error $ "ERROR: Board.newBoard must have positive dimensions: " ++ (show (nr, nc))
  | length rgvs /= nr || length cgvs /= nc || length dvs /= nr =
    error $ "ERROR: Board.newBoard received inconsistent arguments"
  | otherwise = Board (nr, nc, ds, rgs, cgs)
    where ds = map (map (\v -> DataTile (v, Unknown))) dvs
          rgs = map (\v -> GoalTile (v, Unsat)) rgvs
          cgs = map (\v -> GoalTile (v, Unsat)) cgvs

-- | Get the number of rows in the Board
getNRows :: Board -> Int
getNRows (Board (nr, _, _, _, _)) = nr

-- | Get the number of columns in the Board
getNCols :: Board -> Int
getNCols (Board (_, nc, _, _, _)) = nc

-- | Check if a row and column are within the Board bounds
inBounds :: Board -> Int -> Int -> Bool
inBounds (Board (nr, nc, _, _, _)) r c = rowInBounds && colInBounds
  where rowInBounds = r >= 0 && r < nr
        colInBounds = c >= 0 && c < nc

-- | Get a DataTile from the Board given a row number and column number
getData :: Board -> Int -> Int -> DataTile
getData (Board (nr, nc, ds, rgs, cgs)) r c = case inBounds (Board (nr, nc, ds, rgs, cgs)) r c of
  True -> ds !! r !! c
  False -> error $ "ERROR: Board.getData OOB @ " ++ (show (r, c))

-- | Get al DataTiles from the Board
getAllData :: Board -> [[DataTile]]
getAllData (Board (_, _, ds, _, _)) = ds

-- | Replace an element in a list with another element at an index
replace :: [a] -> a -> Int -> [a]
replace xs e i = take i xs ++ [e] ++ drop (i + 1) xs

-- | Set a DataTile in the Board given a row number, a column number, and the DataTile
setData :: Board -> Int -> Int -> DataTile -> Board
setData (Board (nr, nc, ds, rgs, cgs)) r c t = case inBounds (Board (nr, nc, ds, rgs, cgs)) r c of
  True -> Board (nr, nc, nds, rgs, cgs)
    where nds = replace ds (replace (ds !! r) t c) r
  False -> error $ "ERROR: Board.setData OOB @ " ++ (show (r, c))

-- | Get a GoalTile from the Board given a row number
getRowGoal :: Board -> Int -> GoalTile
getRowGoal (Board (nr, _, _, rgs, _)) r = case r >= 0 && r < nr of
  True -> rgs !! r
  False -> error $ "ERROR: Board.getRowGoal OOB @ " ++ (show r)

-- | Get a GoalTile from the Board given a column number
getColGoal :: Board -> Int -> GoalTile
getColGoal (Board (_, nc, _, _, cgs)) c = case c >= 0 && c < nc of
  True -> cgs !! c
  False -> error $ "ERROR: Board.getColGoal OOB @ " ++ (show c)

-- | Get a list of DataTiles for a given row
getRow :: Board -> Int -> [DataTile]
getRow (Board (nr, nc, ds, rgs, cgs)) r = case inBounds (Board (nr, nc, ds, rgs, cgs)) r 0 of
  True -> ds !! r
  False -> error $ "ERROR: Board.getRow OOB @ " ++ (show r)

-- | Get a list of DataTiles for a given column
getCol :: Board -> Int -> [DataTile]
getCol (Board (nr, nc, ds, rgs, cgs)) c = case inBounds (Board (nr, nc, ds, rgs, cgs)) 0 c of
  True -> [getData (Board (nr, nc, ds, rgs, cgs)) r c | r <- [0 .. (nr - 1)]]
  False -> error $ "ERROR: Board.getCol OOB @ " ++ (show c)

-- | Set a row of DataTiles in the Board
setRow :: Board -> [DataTile] -> Int -> Board
setRow (Board (nr, nc, ds, rgs, cgs)) row r = Board (nr, nc, (replace ds row r), rgs, cgs)

-- | Set a column of DataTiles in the Board
setCol :: Board -> [DataTile] -> Int -> Board
setCol (Board (nr, nc, ds, rgs, cgs)) col c = Board (nr, nc, nds, rgs, cgs)
  where nds = map (\(colValue, row) -> replace row colValue c) (zip col ds)

-- | Construct a new DataTile
newDataTile :: Int -> DataState -> DataTile
newDataTile v s = DataTile (v, s)

-- | Get the value of a DataTile
getDataValue :: DataTile -> Int
getDataValue (DataTile (v, _)) = v

-- | Get the state of a DataTile
getDataState :: DataTile -> DataState
getDataState (DataTile (_, s)) = s

-- | Construct a new GoalTile
newGoalTile :: Int -> GoalState -> GoalTile
newGoalTile v s = GoalTile (v, s)

-- | Get the value of a GoalTile
getGoalValue :: GoalTile -> Int
getGoalValue (GoalTile (v, _)) = v

-- | Get the state of a GoalTile
getGoalState :: GoalTile -> GoalState
getGoalState (GoalTile (_, s)) = s
