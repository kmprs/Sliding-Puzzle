module PuzzleLogic.PuzzleLogic where
import PuzzleLogic.Types

{-|
 - Next State Function: 
 - @brief: A state in a 2D array can have up to four different neighbor states. 
 -         These states are being calculated by this function. Please consider, 
 -         that the number of states is based on the position in the array. If the
 -         current '0' is at the edge or corner there are less than 4 new states. 
 - @datatype State: basically a 2D list of Ints -> see 'src/PuzzleLogic/Types.hs'
 - @param state: current state of the search algorithm
 - @returns: a list containing with all reachable states 
 -}
nextStates2D :: State -> [State]
nextStates2D state = nextStates2DHelper state [] [(1, 0), (0, 1), ((-1), 0), (0, (-1))]
  where
    -- Runs through all directions at once and checks if the '0' can be moved in that
    -- direction. If so the according new state is added to the output
    nextStates2DHelper :: State -> [State] -> [Position] -> [State]
    nextStates2DHelper _ explored_states [] = explored_states   
    nextStates2DHelper s explored_states (direction:rest) =
        let newState = moveTile s direction
        in case newState of
            Nothing -> nextStates2DHelper s explored_states rest  
            Just value -> nextStates2DHelper s (value:explored_states) rest

{-|
 - @brief: Moves the '0' in one direction and returns the according new state if it is
 - valid. If not, Nothing is being returned. 
 - @param state: current state of the search algorithm
 - @param (dx, dy): Tuple containing the direction in which the '0' moves 
 - @returns: A state or Nothing
 -}
moveTile :: State -> Position -> Maybe State
moveTile state (dx, dy) = do
    (row, col) <- findTile state 0 0
    let newRow = row + dx
    let newCol = col + dy
    if isValidMove state newRow newCol
        then Just (swapTiles state (row, col) (newRow, newCol))
        else Nothing  


{-|
 - @brief: Swaps two tiles in one based on two positions 
 - @param state: current state of the search algorithm
 - @param (r1, c1): Position of the first tile which should be moved
 - @param (r2, c2): Position of the second tile which should be moved
 - @returns: The output state of the swap
 -}
swapTiles :: State -> Position -> Position -> State
swapTiles state (r1, c1) (r2, c2) =
    [[swap r c | (c, _) <- zip [0..] row] | (r, row) <- zip [0..] state]
  where
    val1 = (state !! r1) !! c1
    val2 = (state !! r2) !! c2
    swap r c
        | (r, c) == (r1, c1) = val2
        | (r, c) == (r2, c2) = val1
        | otherwise = (state !! r) !! c
    

{-|
 - @brief: Finds the position of the tile which is being searched for 
 -         (most of the time this tile is '0')
 - @param (row:rest): current state of the search algorithm
 - @param target: the tile of which the position shall be returned
 - @param rowIndex: A accumulative variable which stores the information of the current
 - row 
 - @returns: The tile's position if it was found (else Nothing) 
 -}
findTile :: State -> Int -> Int -> Maybe (Int, Int)
findTile [] _ _ = Nothing  
findTile (row:rest) target rowIndex =
    case searchInRow target rowIndex 0 row of
        Just pos -> Just pos
        Nothing  -> findTile rest target (rowIndex + 1)
        
        
{-|
 - @brief: Finds a tile inside of a row
 -         (most of the time this tile is '0')
 - @param target: the tile of which the position shall be returned
 - @param rowIndex: Index of the current row
 - @param colIndex: A accumulative variable which stores the information of the current
 - column 
 - @param (x:xs): The remaining row (which was not traversed before) 
 - @returns: The tile's position if it was found (else Nothing) 
 -}
searchInRow :: Int -> Int -> Int -> Row  -> Maybe Position
searchInRow _ _ _ [] = Nothing
searchInRow target rowIndex colIndex (x:xs)
    | x == target = Just (rowIndex, colIndex)
    | otherwise = searchInRow target rowIndex (colIndex + 1) xs

{-|
 - @brief: checks if a given tile position is in bound
 - @param state: The current state which shall be checked (relevant for the boundaries)
 - @param newRow: Index inside the row
 - @param state: Index inside the column
 - @returns: True if the new tile position is valid (else False) 
 -}
isValidMove :: State -> Int -> Int -> Bool
isValidMove [] _ _ = False
isValidMove state newRow newCol =
    newRow >= 0 && newRow < length state &&
    case state of
      (firstRow:_) -> newCol >= 0 && newCol < length firstRow

