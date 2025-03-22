module Main where
import Helpers
import BFS


main :: IO ()
main = do 
    let row1 = [1, 2, 3, 4]
    let row2 = [5, 6, 7, 8]
    let row3 = [9, 10, 11, 12]
    let row4 = [13, 14, 15, 0]

    let row5 = [8, 1, 3, 9]
    let row6 = [2, 13, 4, 5]
    let row7 = [12, 15, 7, 6]
    let row8 = [14, 10, 11, 0]

    let start  = [row1, row2, row3, row4]
    let target = [row5, row6, row7, row8] 
    let path = bfs start target  
 
    case path of
        Just p -> printPath p
        Nothing -> putStrLn "Path is missing"

