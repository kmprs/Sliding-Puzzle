module Search.AStar (astar) where
import qualified Data.Heap as Hp
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)

astar :: (Hashable s, Ord a, Num a)
      => s -> s -> (s -> [s]) -> (s -> s -> a) -> Maybe [s]
astar start goal nextFunc heuristic 
 = (fmap reverse) (searchLogic goal nextFunc heuristic HS.empty (Hp.singleton (heuristic start goal, [start])))

searchLogic :: (Hashable s, Ord a, Num a)
            => s -> (s -> [s]) -> (s -> s -> a) -> HS.HashSet s -> Hp.MinPrioHeap a [s] -> Maybe [s]
searchLogic goal nextFunc heuristic visited prioQueue
  | Hp.null prioQueue = Nothing
  | otherwise = let
      (current, path, restQueue) = getNext prioQueue visited
    in if current == goal then Just path
    else let
        newVisited = (HS.insert current visited)
        newQueue = updateQueue path goal (nextFunc current) heuristic restQueue
      in searchLogic goal nextFunc heuristic newVisited newQueue

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

updateQueue :: (Hashable s, Ord a, Num a)
            => [s] -> s -> [s] -> (s -> s -> a) -> Hp.MinPrioHeap a [s] -> Hp.MinPrioHeap a [s]
updateQueue _ _ [] _ prioQueue = prioQueue
updateQueue currentPath goal neighbours heuristic prioQueue
 = updateQueue currentPath goal ns heuristic newQueue
   where 
    (n:ns) = neighbours
    prio = fromIntegral (length currentPath) + heuristic n goal
    newQueue = Hp.insert (prio, n:currentPath) prioQueue
            
