module Main where
import PuzzleLogic.Helpers (isSolvable, printPath, printState)
import PuzzleLogic.PuzzleLogic (nextStates2D)
import PuzzleLogic.Heuristic (manhattan)
import Search.BFS (bfs) 
import Search.AStar (astar)


main :: IO ()
main = do 
    let start3 = [[6, 7, 8],
                  [4, 1, 2],
                  [3, 5, 0]]

    let target3 = [[1, 2, 3],
                   [4, 5, 6],
                   [7, 8, 0]]

    let start4 = [[5,  1,  7,  3],
                  [9,  2, 11,  4],
                  [13, 6, 15,  8],
                  [0, 10, 14, 12]]

    let target4 = [[1,  2,  3,  4],
                   [5,  6,  7,  8],
                   [9, 10, 11, 12],
                   [13,14, 15,  0]]

    putStrLn "Solving a 3x3 sliding puzzle with starting configuration:"
    printState start3
    if isSolvable start3 target3
        then do
            putStrLn "Using BFS:"
            case bfs start3 target3 nextStates2D of
                Nothing -> putStrLn "Couldn't find a path to the target."
                Just path -> printPath path
            putStrLn "Using A*:"
            case astar start3 target3 nextStates2D manhattan of
                Nothing -> putStrLn "Couldn't find a path to the target."
                Just path -> printPath path
        else putStrLn "Configuration is not solvable."
    putStrLn "Solving a 4x4 sliding puzzle with starting configuration:"
    printState start4
    if isSolvable start4 target4
        then do
            putStrLn "Using BFS:"
            case bfs start4 target4 nextStates2D of
                Nothing -> putStrLn "Couldn't find a path to the target."
                Just path -> printPath path
            putStrLn "Using A*:"
            case astar start4 target4 nextStates2D manhattan of
                Nothing -> putStrLn "Couldn't find a path to the target."
                Just path -> printPath path
        else putStrLn "Configuration is not solvable."

