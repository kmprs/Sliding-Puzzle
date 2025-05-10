module PuzzleLogic.Helpers where
import PuzzleLogic.Types
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Array 


{-|
 - @brief: prints the path 
 - @param path: Path which shall be printed out
 - @returns: No return value, just a print statement
 -}
printPath :: Path -> IO() 
printPath [] = return ()
printPath [first] = printState first 
printPath (first:next) = do 
    printState first  
    printPath next

{-|
 - JUST A DEBUG METHOD, THIS METHOD DOES NOT CONTRIBUTE THE BUSINESS LOGIC!
 -}
printState :: State -> IO () 
printState [] = return ()
printState [first] = do
    printRow first
    putStrLn ""
printState (first:next) = do 
    printRow first 
    printState next

{-|
 - JUST A DEBUG METHOD, THIS METHOD DOES NOT CONTRIBUTE THE BUSINESS LOGIC!
 -}
printRow :: Row -> IO() 
printRow [] = return () 
printRow [first] = putStrLn $ show first
printRow (first:next) = putStr (show first ++ ",") >> printRow next


{-|
 - @brief: checks if a given puzzle is mathematically solvable -> avoids infinite loops
 -         (see https://de.m.wikipedia.org/wiki/15-Puzzle#Mathematischer_Hintergrund)
 - @param start: starting state
 - @param target: target state
 - @returns: True if the puzzle is solveable (else False)
 -}
isSolvable :: State -> State -> Bool 
isSolvable start target =  
    let startInversions = countInversions $ flatten start
        targetInversions = countInversions $ flatten target
        startBlankRow = fromMaybe 0 (findBlankRow start)
        targetBlankRow = fromMaybe 0 (findBlankRow target)
    in (startInversions + startBlankRow) `mod` 2 == (targetInversions + targetBlankRow) `mod` 2


{-|
 - @brief: An inversion is a pair (arr[i], arr[j]) where i < j, arr[i] > arr[j], and both values are nonzero.
 - @param: List of Ints of which the possible inversions shall be counted  
 - @returns: The count of the inversions
 -}
countInversions :: [Int] -> Int
countInversions arr = length 
    [ (x, y) | (i, x) <- indexed, (j, y) <- indexed, i < j, x > y, x /= 0, y /= 0 ]
  where
    indexed :: [(Int, Int)]
    indexed = zip [0 :: Int ..] arr


{-|
 - @brief: Flattens a 2D array to a 1D array
 - @param(row:rest) : input state (basically a 2D array)  
 - @returns: A list of Ints
 -}
-- 
flatten :: State -> [Int]
flatten [] = []
flatten (row:rest) = row ++ flatten rest 


{-|
 - @brief: Finds the row number of the blank space (0) from the bottom.
 - @param state: input state (basically a 2D array)  
 - @returns: The row index from the bottom (1-based).
 -}
findBlankRow :: State -> Maybe Int
findBlankRow grid =
    let maybeRowIndex = listToMaybe [r | (r, row) <- zip [0..] grid, 0 `elem` row]
        gridHeight = length grid
    in fmap (\rowIndex -> gridHeight - rowIndex) maybeRowIndex

{-|
 - @brief: Transforms a state into a list of positions (row, column), where the index
 - in the list corresponds to the tile number.
 - @return: A list with a position (row, column; starting at zero) for each tile number. 
 -}
toPosList :: State -> [Position]
toPosList s = elems $ array (0,n) [(val, (y, x)) | (y, row) <- zip [0..] s, (x, val) <- zip [0..] row]
    where n = (length $ concat s) - 1 