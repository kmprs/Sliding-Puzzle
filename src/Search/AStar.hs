module Search.AStar (astar, astarWithWeight) where
import qualified Data.Heap as Hp
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)

{-|
- This is the implementation of the A* algorithm. 
- It calls the weighted version of the algorithm with a weight of 1.
- @datatype s:      The type of the state. It must be hashable.
- @datatype a:      The type returned by the heuristiv function. It must be orderable and numeric.
- @param start:     The starting state of the search.
- @param goal:      The target state of the search.
- @param nextFunc:  Function that returns the next possible states from the current state.
- @param heuristic: Function that calculates the heuristic distance between two states.
- @returns:         A list containing the states of the shortest path possible or Nothing if no path can be found.
-}
astar :: (Hashable s, Ord a, Num a)
      => s -> s -> (s -> [s]) -> (s -> s -> a) -> Maybe [s]
astar start goal nextFunc heuristic = astarWithWeight start goal nextFunc heuristic 1

{-|
- This is the implementation of the A* algorithm with a weight parameter. 
- It calls the searchLogic function with an empty "visited" set and a priority queue that contains the starting state.
- @datatype s:      The type of the state. It must be hashable.
- @datatype a:      The type returned by the heuristiv function. It must be orderable and numeric.
- @param start:     The starting state of the search.
- @param goal:      The target state of the search.
- @param nextFunc:  Function that returns the next possible states from the current state.
- @param heuristic: Function that calculates the heuristic distance between two states.
- @param weight:    A weight value that can speed up the calculation at the cost of possibly non optimal 
                    solution. Must be at least 1.
- @returns:         A list containing the states of the shortest path possible or Nothing if no path can be found.
-}
astarWithWeight :: (Hashable s, Ord a, Num a)
      => s -> s -> (s -> [s]) -> (s -> s -> a) -> a -> Maybe [s]
astarWithWeight start goal nextFunc heuristic weight
 | weight < 1 = error "Weight must be at least 1."
 | otherwise
   = (fmap reverse) (searchLogic goal nextFunc heuristic HS.empty (Hp.singleton (heuristic start goal, [start])) weight)

{-|
- This function contains the actual search loop. It calls itself recursively.
- @datatype s:      The type of the state. It must be hashable.
- @datatype a:      The type returned by the heuristiv function. It must be orderable and numeric.
- @param goal:      The target state of the search.
- @param nextFunc:  Function that returns the next possible states from the current state.
- @param heuristic: Function that calculates the heuristic distance between two states.
- @param visited:   A hashset containing the states that have already been visited.
- @param prioQueue: A priority queue containing the states to be visited.
- @param weight:    A weight value that can speed up the calculation at the cost of possibly non optimal 
                    solution. Must be at least 1.
- @returns:         A list containing the states of the shortest path possible or Nothing if no path can be found.
                    This is the case if the priority queue runs out without having reached the goal.
-}
searchLogic :: (Hashable s, Ord a, Num a)
            => s -> (s -> [s]) -> (s -> s -> a) -> HS.HashSet s -> Hp.MinPrioHeap a [s] -> a -> Maybe [s]
searchLogic goal nextFunc heuristic visited prioQueue weight
  | Hp.null prioQueue = Nothing
  | otherwise = let
      (current, path, restQueue) = getNext prioQueue visited
    in if current == goal then Just path
    else let
        newVisited = (HS.insert current visited)
        newQueue = updateQueue path goal (nextFunc current) heuristic restQueue weight
      in searchLogic goal nextFunc heuristic newVisited newQueue weight

{-|
- This function gets the next state from the priority queue that has not been visited yet.
- @datatype s:      The type of the state. It must be hashable.
- @datatype a:      The type returned by the heuristiv function. It must be orderable and numeric.
- @param queue:     The priority queue containing the states to be visited.
- @param visited:   A hashset containing the states that have already been visited.
- @returns:         The next state, the path to this state and the remaining priority queue.
-}
getNext :: (Hashable s, Ord a, Num a)
        => Hp.MinPrioHeap a [s] -> HS.HashSet s -> (s, [s], Hp.MinPrioHeap a [s])
getNext queue visited = let
    ((_, path), restQueue) = fromJust $ Hp.view queue
  in case path of
    (current:_) -> 
      if HS.member current visited
      then getNext restQueue visited
      else (current, path, restQueue)
    [] -> error "Unexpected empty path in getNext"

{-|
- This function updates the given priority queue by inserting the neighbours (or more precisely, the path
- thereto) of the current state with the respective heuristic distance to the goal as the priority.
- @datatype s:        The type of the state. It must be hashable.
- @datatype a:        The type returned by the heuristiv function. It must be orderable and numeric.
- @param currentPath: The path to the current state.
- @param goal:        The target state of the search.
- @param neighbours:  The neighbours of the current state.
- @param heuristic:   Function that calculates the heuristic distance between two states.
- @param prioQueue:   The priority queue containing the states to be visited.
- @param weight:      A weight value that can speed up the calculation at the cost of possibly non optimal 
                      solution. Must be at least 1.
- @returns:           The updated priority queue.
-}
updateQueue :: (Hashable s, Ord a, Num a)
            => [s] -> s -> [s] -> (s -> s -> a) -> Hp.MinPrioHeap a [s] -> a -> Hp.MinPrioHeap a [s]
updateQueue _ _ [] _ prioQueue _ = prioQueue
updateQueue currentPath goal neighbours heuristic prioQueue weight
 = updateQueue currentPath goal ns heuristic newQueue weight
   where 
    (n:ns) = neighbours
    prio = fromIntegral (length currentPath) +  weight * heuristic n goal
    newQueue = Hp.insert (prio, n:currentPath) prioQueue
            
