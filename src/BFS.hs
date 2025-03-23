module BFS where
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet

import Debug.Trace (trace) 

bfs :: (Eq state, Hashable state) => state -> state -> (state -> [state]) -> Maybe [state] 
bfs start target next_states 
    | start == target = Just [start]
    | otherwise = searchLogic target (HashMap.singleton start start) (Seq.singleton start) (HashSet.singleton start) 
  where 
    searchLogic final parents queue visited 
        | Seq.null queue = Nothing 
        | final == current = Just (extractPath current parents) -- target found
        | otherwise =  
            let new_visited = HashSet.insert current visited
                new_states = removeVisited (next_states current) new_visited  
                new_parents = addToParents current new_states parents
                new_queue = rest Seq.>< Seq.fromList new_states
            in searchLogic final new_parents new_queue new_visited
      where 
        (current Seq.:< rest) = Seq.viewl queue 
             

insertToVisited :: (Eq state, Hashable state) => [state] -> HashSet.HashSet state -> HashSet.HashSet state
insertToVisited items set = foldr HashSet.insert set items


removeVisited :: (Eq state, Hashable state) => [state] -> HashSet.HashSet state -> [state]
removeVisited states visited = filter (\x -> not (HashSet.member x visited)) states
    

addToParents :: (Eq state, Hashable state) => state -> [state] -> HashMap.HashMap state state -> HashMap.HashMap state state
addToParents _ [] parents = parents
addToParents parent (child:children) parents = addToParents parent children new_parents
  where
      new_parents = HashMap.insertWith (\_ old -> old) child parent parents
        

extractPath :: (Eq state, Hashable state) => state -> HashMap.HashMap state state -> [state]
extractPath current parents = reverse (extractPathHelper current parents [])
  where
    extractPathHelper :: (Eq state, Hashable state) => state -> HashMap.HashMap state state -> [state] -> [state]
    extractPathHelper current parents acc =
        case HashMap.lookup current parents of
            Just parent
                | current == parent -> current : acc
                | otherwise -> extractPathHelper parent parents (current : acc)
            Nothing -> current : acc


