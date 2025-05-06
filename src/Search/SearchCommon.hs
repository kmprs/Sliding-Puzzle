{-# LANGUAGE ScopedTypeVariables #-}

module Search.SearchCommon where
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)

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