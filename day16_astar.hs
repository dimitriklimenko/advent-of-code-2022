#!/usr/bin/env runhaskell

import Data.Array (bounds, indices, (!))
import Data.Either.Combinators (fromRight')
import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.List (singleton, sortOn)
import Data.List.Split (chunksOf)
import Data.Ord (Down(..))
import qualified Data.Heap as Heap
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Parsec (Parsec, endBy, many1, parse, sepBy, string, try, (<|>))
import Text.Parsec.Char (digit, newline, upper)

import GraphUtils (DistanceMatrix, distanceMatrix)

-- Types and constructors / destructors
-- Node stores flow rate of the valve, label, and neighbour labels.
type Node = (Int, String, [String])

type GraphResult = (Graph, Vertex -> Node, String -> Maybe Vertex)

-- Parsing
number :: Parsec String () Int
number = read <$> many1 digit

name :: Parsec String () String
name = many1 upper

nodeParser :: Parsec String () Node
nodeParser = do
    key <- string "Valve " >> name
    flowRate <- string " has flow rate=" >> number
    tunnels <- try (string "; tunnels lead to valves " >> sepBy name (string ", ")) <|>
        (string "; tunnel leads to valve " >> singleton <$> name)
    return (flowRate, key, tunnels)

inputParser :: Parsec String () [Node]
inputParser = endBy nodeParser newline

loadGraph :: String -> GraphResult
loadGraph = graphFromEdges . fromRight' . parse inputParser ""

-- Searching
data TransitState = Neither | You Int | Elephant Int
    deriving (Eq, Ord, Show)

data State = State
    { closedValves :: Map.Map Vertex Int
    , location :: Vertex
    , elephantLocation :: Vertex
    , transitState :: TransitState
    , timeRemaining :: Int
    , availableFlowRate :: Int
    } deriving (Eq, Ord, Show)

initState :: GraphResult -> Int -> State
initState (graph, nodeFromVertex, vertexFromKey) timeLeft =
    State valves start start Neither timeLeft afr
  where
    Just start = vertexFromKey "AA"
    vertices   = indices graph
    valves     = Map.fromList [(v, fr) | v <- vertices, let (fr, _, _) = nodeFromVertex v, fr > 0]
    afr        = sum . Map.elems $ valves

successors :: Bool -> DistanceMatrix -> State -> [(Int, State)]
successors hasElephant matrix (State valves you ele ts time afr) =
    (0, State valves you ele Neither 0 afr) : youMoves ++ elephantMoves
  where
    youMoves = case ts of
        You time' -> [(0, State valves you ele Neither time' afr)]
        _ -> [(time' * fr, State (Map.delete next valves) next ele nextTransitState nextTime (afr - fr))
            | (next, fr) <- Map.toList valves
            , let Just dist = (matrix ! you) ! next
            , let time' = time - dist - 1
            , time' >= 0
            , let (nextTime, nextTransitState) = case ts of
                    Neither -> (time, You time')
                    Elephant eleTime
                        | eleTime > time'  -> (eleTime, You time')
                        | eleTime == time' -> (time', Neither)
                        | otherwise        -> (time', Elephant eleTime)]
    elephantMoves = if not hasElephant then [] else case ts of
        Elephant time' -> [(0, State valves you ele Neither time' afr)]
        _ -> [(time' * fr, State (Map.delete next valves) you next nextTransitState nextTime (afr - fr))
            | (next, fr) <- Map.toList valves
            , let Just dist = (matrix ! ele) ! next
            , let time' = time - dist - 1
            , time' >= 0
            , let (nextTime, nextTransitState) = case ts of
                    Neither -> (time, Elephant time')
                    You youTime
                        | youTime > time'  -> (youTime, Elephant time')
                        | youTime == time' -> (time', Neither)
                        | otherwise        -> (time', You youTime)]

heuristic1 :: State -> Int
heuristic1 (State valves _ _ _ t afr) = t * afr

heuristic2 :: Bool -> State -> Int
heuristic2 hasElephant (State valves _ _ _ t afr) = foldr helper 0 . zip [t,t-2..] $ flowBumps
  where
    flowBumps = (if hasElephant then map sum . chunksOf 2 else id) . sortOn Down . Map.elems $ valves
    helper :: (Int, Int) -> Int -> Int
    helper (-1, fr) _ = 0
    helper (0,  fr) _ = 0
    helper (t,  fr) c = t * fr + c

aStarSearch :: (State -> [(Int, State)]) -> (State -> Int) -> Heap.MaxPrioHeap Int (Int, State) -> Int
aStarSearch succFunc heuristic heap
    | timeRemaining state == 0 = p
    | otherwise                = aStarSearch succFunc heuristic newHeap
    where
      Just ((_, (p, state)), rest) = Heap.view heap
      newHeap = rest `Heap.union` Heap.fromList [(p' + heuristic ns, (p', ns)) | (dp, ns) <- succFunc state, let p' = p + dp]

aStarSearchInit :: (State -> [(Int, State)]) -> (State -> Int) -> State -> Int
aStarSearchInit succFunc heuristic state0 = aStarSearch succFunc heuristic initHeap
  where
    initHeap = Heap.singleton (heuristic state0, (0, state0))

findMaxPressureRelease :: Bool -> GraphResult -> Int
findMaxPressureRelease hasElephant result@(graph, _, _) = aStarSearchInit succ heuristic state0
  where
    matrix = distanceMatrix (bounds graph) (graph !)
    state0 = initState result (if hasElephant then 26 else 30)
    succ = successors hasElephant matrix
    heuristic = heuristic2 hasElephant

main :: IO ()
main = do
    args <- getArgs
    let hasElephant = not $ null args
    print . findMaxPressureRelease hasElephant . loadGraph =<< getContents
