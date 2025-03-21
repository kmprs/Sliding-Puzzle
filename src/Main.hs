module Main where
import qualified Data.Map as Map
import Types
import Helpers
import BFS


main :: IO ()
main = do 
    let row1 = [1, 2, 3]
    let row2 = [4, 5, 6]
    let row3 = [7, 8, 9]
    let state = [row1, row2, row3]

    let path = bfs state state 

    printState state 
    printState state 
