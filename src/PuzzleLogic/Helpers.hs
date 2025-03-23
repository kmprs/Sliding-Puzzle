module PuzzleLogic.Helpers where
import PuzzleLogic.Types


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


isSolvable :: State -> State -> Bool 
isSolvable start target =  
     (countInversions (flatten start)) `mod` 2 
     == (countInversions (flatten target)) `mod`2 

-- An inversion is a pair (arr[i], arr[j]) where i < j, arr[i] > arr[j], 
-- and both values are nonzero.
countInversions :: [Int] -> Int
countInversions arr = length 
    [ (x, y) | (i, x) <- indexed, (j, y) <- indexed, i < j, x > y, x /= 0, y /= 0 ]
  where
    indexed :: [(Int, Int)]
    indexed = zip [0 :: Int ..] arr


flatten :: State -> [Int]
flatten [] = []
flatten (row:rest) = row ++ flatten rest 
