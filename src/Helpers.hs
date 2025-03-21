module Helpers where
import Types

printPath :: Path -> IO() 
printPath [] = return ()
printPath [first] = printState first 
printPath (first:next) = do 
    printState first  
    printPath next

printState :: State -> IO () 
printState [] = return ()
printState [first] = do
    printRow first
    putStrLn ""
printState (first:next) = do 
    printRow first 
    printState next

printRow :: Row -> IO() 
printRow [] = return () 
printRow [first] = putStrLn $ show first
printRow (first:next) = putStr (show first ++ ",") >> printRow next
