{-# LANGUAGE ScopedTypeVariables #-}

module GraphUtils (DistanceMatrix, distancesFrom, distanceMatrix, stepDistMap) where

import Data.Array (Array, array)
import qualified Data.Map as Map (Map, empty, insert, lookup, notMember)
import qualified Data.Set as Set (Set, empty, fromList, null, union)

-- Single step update to a distance map. Takes a successor-generating functon, current distance, and a pair consisting of the current map
--  and a set of positions to add at the current distance. Returns an updated map and the next set of positions.
stepDistMap :: forall state. Ord state => (state -> [state]) -> Int -> (Map.Map state Int, Set.Set state) -> (Map.Map state Int, Set.Set state)
stepDistMap successors dist (dMap, currentStates) = foldr combine (dMap, Set.empty) currentStates
  where
    combine :: state -> (Map.Map state Int, Set.Set state) -> (Map.Map state Int, Set.Set state)
    combine s (dMap, nextStates) = (Map.insert s dist dMap, nextStates `Set.union` Set.fromList (filter (`Map.notMember` dMap) (successors s)))

distancesFrom :: forall state. Ord state => (state -> [state]) -> [state] -> Map.Map state Int
distancesFrom successors startStates = helper 0 (Set.fromList startStates) Map.empty
  where
    helper :: Int -> Set.Set state -> Map.Map state Int -> Map.Map state Int
    helper dist states currentMap
        | Set.null states = currentMap
        | otherwise   = helper (dist+1) nextStates nextMap
      where
        (nextMap, nextStates) = stepDistMap successors dist (currentMap, states)

type DistanceMatrix = Array Int (Array Int (Maybe Int))

distanceMatrix :: (Int, Int) -> (Int -> [Int]) -> DistanceMatrix
distanceMatrix bounds@(lo, hi) successors = array bounds
    [(i, makeDistArray i) | i <- [lo..hi]]
  where
    makeDistArray :: Int -> Array Int (Maybe Int)
    makeDistArray i = array bounds [(j, Map.lookup j (distancesFrom successors [i])) | j <- [lo..hi]]
