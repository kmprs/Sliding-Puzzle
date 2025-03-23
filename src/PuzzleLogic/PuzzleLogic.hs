module PuzzleLogic.PuzzleLogic where
import PuzzleLogic.Types


nextStates2D :: State -> [State]
nextStates2D state = nextStates2DHelper state [] [(1, 0), (0, 1), ((-1), 0), (0, (-1))]
  where
    nextStates2DHelper :: State -> [State] -> [Position] -> [State]
    nextStates2DHelper _ explored_states [] = explored_states  
    nextStates2DHelper state explored_states (direction:rest) =
        let newState = moveTile state direction
        in case newState of
            Nothing -> nextStates2DHelper state explored_states rest  
            Just value -> nextStates2DHelper state (value:explored_states) rest


moveTile :: State -> Position -> Maybe State
moveTile state (dx, dy) = do
    (row, col) <- findTile state 0 0
    let newRow = row + dx
    let newCol = col + dy
    if isValidMove state newRow newCol
        then Just (swapTiles state (row, col) (newRow, newCol))
        else Nothing  


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
    

findTile :: State -> Int -> Int -> Maybe (Int, Int)
findTile [] _ _ = Nothing  
findTile (row:rest) target rowIndex =
    case searchInRow target rowIndex 0 row of
        Just pos -> Just pos
        Nothing  -> findTile rest target (rowIndex + 1)
        
        
searchInRow :: Int -> Int -> Int -> Row  -> Maybe Position
searchInRow _ _ _ [] = Nothing
searchInRow target rowIndex colIndex (x:xs)
    | x == target = Just (rowIndex, colIndex)
    | otherwise = searchInRow target rowIndex (colIndex + 1) xs


isValidMove :: State -> Int -> Int -> Bool
isValidMove state newRow newCol =
    newRow >= 0 && newRow < length state &&
    newCol >= 0 && newCol < length (head state)

