module BFS where
import Types
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set


bfs :: State -> State -> Maybe Path 
bfs start target 
    | start == target = Just [start]
    | otherwise = searchLogic target (Map.singleton start start) (Seq.singleton start) (Set.singleton start) 
  where 
    searchLogic :: State -> Map.Map State State -> Seq.Seq State -> Set.Set State -> Maybe Path
    searchLogic final parents queue visited 
        | Seq.null queue = Nothing 
        | final == current = Just (extractPath current parents) -- target found
        | otherwise =  
            let new_visited = Set.insert current visited  
                new_states = removeVisited (exploreStates current [] directions) new_visited  
                new_parents = addToParents current new_states parents
                new_queue = rest Seq.>< Seq.fromList new_states
            in searchLogic final new_parents new_queue new_visited
      where 
        (current Seq.:< rest) = Seq.viewl queue 
             

insertToVisited :: [State] -> Set.Set State -> Set.Set State
insertToVisited items set = foldr Set.insert set items 


removeVisited :: [State] -> Set.Set State -> [State]
removeVisited states visited = filter (`Set.notMember` visited) states
    

addToParents :: State -> [State] -> Map.Map State State -> Map.Map State State
addToParents _ [] parents = parents
addToParents parent (child:children) parents = addToParents parent children new_parents
  where  
      new_parents = Map.insertWith (\_ old -> old) child parent parents
        

extractPath :: Ord state => state -> Map.Map state state -> [state]
extractPath current parents =
    case Map.lookup current parents of
        Just parent
            | current == parent -> [current]
            | otherwise -> extractPath parent parents ++ [current]
        Nothing -> [current]
    

exploreStates :: State -> [State] -> [Position] -> [State]
exploreStates _ exploredStates [] = exploredStates  
exploreStates state exploredStates (direction:rest) =
    let newState = moveTile state direction
    in case newState of
        Nothing -> exploreStates state exploredStates rest  
        Just value -> exploreStates state (value:exploredStates) rest  


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

