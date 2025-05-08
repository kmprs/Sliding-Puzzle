{-# LANGUAGE ScopedTypeVariables #-}

module Search.AStar (astar) where
import Search.SearchCommon (extractPath)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashPSQ as HPSQ
import Data.Maybe (fromJust, isJust, isNothing)

data StateInfo s a = StateInfo {
  state :: s, -- the state itself
  f :: a, -- total cost (g + h)
  g :: a, -- cost from start to current node
  h :: a, -- heuristic cost from current node to target
  parent :: Maybe s -- parent node
} deriving Show

type StateQueue s a = HPSQ.HashPSQ s a (StateInfo s a)

instance Ord s => Ord (StateInfo s a) where
  compare x y = compare (state x) (state y)

instance Eq s => Eq (StateInfo s a) where
  x == y = state x == state y

astar :: (Eq s, Hashable s, Ord s, Num a, Ord a) => s -> s -> (s -> [s]) -> (s -> s -> a) -> Maybe [s]
astar start target nextFunc heuristic 
 | start == target = Just [start]
 | not (HM.member target parentMap) = Nothing
 | otherwise = Just $ extractPath target parentMap
    where parentMap = getParentMap start target nextFunc heuristic

getParentMap :: (Eq s, Hashable s, Ord s, Num a, Ord a) => s -> s -> (s -> [s]) -> (s -> s -> a) -> HM.HashMap s s
getParentMap start target nextFunc heuristic 
  = HM.fromList [(state stateInfo, fromJust p) | stateInfo <- stateInfos, let p = parent stateInfo, isJust p]
  where startInfo = StateInfo start 0 0 0 Nothing
        open = HPSQ.singleton start 0 startInfo
        stateInfos = searchLogic startInfo target nextFunc heuristic open []

searchLogic :: (Eq s, Hashable s, Ord s, Num a, Ord a) => StateInfo s a -> s -> (s -> [s]) -> (s -> s -> a) -> StateQueue s a -> [StateInfo s a] -> [StateInfo s a]
searchLogic current target nextFunc heuristic open closed 
 | HPSQ.null open || state current == target = closed
 | otherwise = searchLogic next target nextFunc heuristic newOpen newClosed
   where
    (_, _, next, restOpen) = fromJust $ HPSQ.minView open
    newClosed = current : closed
    neighbours = filter (\n->notElem n (map (\c->state c) closed)) $ nextFunc $ state current
    newOpen = insertNeighbours restOpen neighbours heuristic (state current) (g current) target

insertNeighbours :: (Eq s, Hashable s, Ord s, Num a, Ord a) => StateQueue s a -> [s] -> (s -> s -> a) -> s -> a -> s -> StateQueue s a
insertNeighbours open [] _ _ _ _ = open
insertNeighbours open (n:ns) heuristic current current_g goal = insertNeighbours newOpen ns heuristic current current_g goal
  where 
    new_g = current_g + heuristic current n
    new_h = heuristic n goal
    new_f = new_g + new_h
    newInfo = StateInfo n new_f new_g new_h (Just current)
    present = HPSQ.lookup n open
    newOpen = if isNothing present
      then HPSQ.insert n new_f newInfo open
      else let (_, presentInfo) = fromJust present
        in if new_g < g presentInfo
          then HPSQ.insert n new_f newInfo open
        else open