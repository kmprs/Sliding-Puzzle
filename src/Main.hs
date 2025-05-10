module Main where
import PuzzleLogic.Helpers (isSolvable, printPath)
import PuzzleLogic.PuzzleLogic (nextStates2D)
import PuzzleLogic.Heuristic (manhattan)
import Search.BFS (bfs) 
import Search.AStar (astar)


main :: IO ()
main = do 
    let start = [[6, 7, 8],
                 [4, 1, 2],
                 [3, 5, 0]]

    -- let start = [[1, 2, 3],
    --               [4, 5, 6],
    --               [7, 0, 8]]

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
        then do
            putStrLn "Using BFS:"
            case bfs start target nextStates2D of
                Nothing -> putStrLn "Couldn't find a path to the target."
                Just path -> printPath path
            putStrLn "Using A*:"
            case astar start target nextStates2D manhattan of
                Nothing -> putStrLn "Couldn't find a path to the target."
                Just path -> printPath path
        else putStrLn "Configuration is not solvable."

