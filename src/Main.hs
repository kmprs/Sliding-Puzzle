module Main where
import PuzzleLogic.Helpers (isSolvable, printPath)
import PuzzleLogic.PuzzleLogic (nextStates2D)
import BFS


main :: IO ()
main = do 
    let start = [[6, 7, 8],
                 [4, 1, 2],
                 [3, 5, 0]]

    let target = [[1, 2, 3],
                  [4, 5, 6],
                  [7, 8, 0]]

--    let start = [[1,  2,  3,  4],
--                 [5,  6,  7,  8],
--                 [9, 10, 11, 12],
--                 [13, 15, 14, 0]]
--
--    let target = [[1,  2,  3,  4],
--                  [5,  6,  7,  8],
--                  [9, 10, 11, 12],
--                  [13, 14, 15, 0]]

    if isSolvable start target 
        then 
            maybe (putStrLn "Couldn't find a path to the target.") 
            -- putStrLn (Just "Puzzle is solveable.")
            printPath (bfs start target nextStates2D) 
        else putStrLn "Configuration is not solvable."

