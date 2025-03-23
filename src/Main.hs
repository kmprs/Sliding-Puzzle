module Main where
import PuzzleLogic.Helpers (isSolvable, printPath)
import PuzzleLogic.PuzzleLogic (nextStates2D)
import BFS


main :: IO ()
main = do 
    let row1 = [1, 2, 3]
    let row2 = [4, 5, 6]
    let row3 = [7, 0, 8]

    let row4 = [2, 6, 7]
    let row5 = [3, 8, 5]
    let row6 = [4, 1, 0]

    let start  = [row1, row2, row3] 
    let target = [row4, row5, row6] 

    if isSolvable start target 
        then 
            maybe (putStrLn "Couldn't find a path to the target.") 
            -- putStrLn (Just "Puzzle is solveable.")
            printPath (bfs start target nextStates2D) 
        else putStrLn "Configuration is not solvable."

