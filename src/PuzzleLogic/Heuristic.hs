module PuzzleLogic.Heuristic where

import PuzzleLogic.Types (State)
import PuzzleLogic.Helpers (toPosList)

{-|
- @brief: calculates the manhattan distance between two states
- @param s1: first state
- @param s2: second state
- @return Σ (|x1 - x2| + |y1 - y2|)
-}
manhattan :: State -> State -> Int
manhattan s1 s2 = 
    sum [abs (x1 - x2) + abs (y1 - y2) | ((y1, x1), (y2, x2)) <- zip s1PosList s2PosList]
    where
        s1PosList = toPosList s1
        s2PosList = toPosList s2