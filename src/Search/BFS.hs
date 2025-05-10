{-# LANGUAGE ScopedTypeVariables #-}


module Search.BFS (bfs) where
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet


{-|
 - Breadth-First-Search: 
 - @datatype state: any datatype which can be compared and which is hashable
 - @param start: The starting position of the search
 - @param target: The target position of the search
 - @param next_states: Function which provides a logic for calculating the next possible
 - states
 - @returns: a list containing the states of the shortest path possible
 -}
bfs :: forall state. (Eq state, Hashable state) => state -> state -> (state -> [state]) -> Maybe [state] 
bfs start target next_states 
    | start == target = Just [start]
    | otherwise = searchLogic target 
                              (HashMap.singleton start start) 
                              (Seq.singleton start) 
                              (HashSet.singleton start) 
  where 
    searchLogic :: (Eq state, Hashable state) => 
                   state -> 
                   HashMap.HashMap state state -> 
                   Seq.Seq state -> 
                   HashSet.HashSet state ->
                   Maybe [state]
    searchLogic final parents queue visited 
        | Seq.null queue = Nothing 
        | otherwise =
            case Seq.viewl queue of
                Seq.EmptyL -> error "Invariant violation: queue should not be empty"
                current Seq.:< rest ->
                    if final == current then
                        Just (extractPath current parents)
                    else 
                        let new_visited = HashSet.insert current visited
                            new_states  = removeVisited (next_states current) new_visited  
                            new_parents = addToParents current new_states parents
                            new_queue   = rest Seq.>< Seq.fromList new_states
                        in searchLogic final new_parents new_queue new_visited
             

{-|
 - @brief: removes items from a hashset which were already marked as visited
 - @datatype state: any datatype which can be compared and which is hashable
 - @param states: list of new possible states
 - @param visited: hashset containing  
 - @returns: updated set of visited states
 -}
removeVisited :: (Eq state, Hashable state) => [state] -> HashSet.HashSet state -> [state]
removeVisited states visited = filter (\x -> not (HashSet.member x visited)) states
    

{-|
 - @brief: adds entries to the hashmap of parents based on the parent and the list of
 - children states
 - @datatype state: any datatype which can be compared and which is hashable
 - @param parent: parent of all children specified
 - @param (child:children): list of children states  
 - @param parents: hashmap containing the parent-child relationsship  
 - @returns: the updated hashmap with the containing the parent-child relationship 
 -}
addToParents :: (Eq state, Hashable state) => state -> [state] -> HashMap.HashMap state state -> HashMap.HashMap state state
addToParents _ [] parents = parents
addToParents parent (child:children) parents = addToParents parent children new_parents
  where
      new_parents = HashMap.insertWith (\_ old -> old) child parent parents
        
{-|
 - @brief: extracts a path based on a parent-child relationship map and a starting node  
 - @datatype state: any datatype which can be compared and which is hashable
 - @param current: starting state of the search. Normally, this should match the final
 - position of the BFS
 - @param parents: hashmap containing the parent-child relationsship  
 - @returns: a list of states which represents calculated path
 -}
extractPath :: (Eq state, Hashable state) => state -> HashMap.HashMap state state -> [state]
extractPath current parents = extractPathHelper current parents []
  where
    extractPathHelper :: (Eq state, Hashable state) => state -> HashMap.HashMap state state -> [state] -> [state]
    extractPathHelper c p acc =
        case HashMap.lookup c p of
            Just parent
                | c == parent -> c : acc
                | otherwise -> extractPathHelper parent p (c : acc)
            Nothing -> c : acc


