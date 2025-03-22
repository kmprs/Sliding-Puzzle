module Main where
import Helpers
import BFS


main :: IO ()
main = do 
    let row1 = [1, 4, 7]
    let row2 = [5, 8, 3]
    let row3 = [2, 0, 6]

    let row4 = [1, 2, 3]
    let row5 = [4, 5, 6]
    let row6 = [7, 8, 0]

    let start  = [row1, row2, row3] 
    let target = [row4, row5, row6] 

    if isSolvable start target 
        then 
            maybe (putStrLn "Couldn't find a path to the target.") 
            -- putStrLn (Just "Puzzle is solveable.")
            printPath (bfs start target) 
        else putStrLn "Configuration is not solvable."
