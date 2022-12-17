#!/usr/bin/env runhaskell

import Control.Monad  (join)
import Data.Array (bounds, indices, (!))
import Data.Bifunctor (bimap)
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

-- Solution via recursive dynamic progarmming
maxPressureRelease :: DistanceMatrix -> Int -> Vertex -> Map.Map Vertex Int -> Int
maxPressureRelease _ 0 _ _ = 0
maxPressureRelease matrix time location valves = maximum $ 0 :
    [ time' * fr + maxPressureRelease matrix time' next (Map.delete next valves)
    | (next, fr) <- Map.toList valves
    , let Just dist = (matrix ! location) ! next
    , let time' = time - dist - 1
    , time' >= 0]

solve :: Bool -> GraphResult -> Int
solve hasElephant (graph, nodeFromVertex, vertexFromKey) =
    if not hasElephant
    then maxPressureRelease matrix 30 start valves
    else maximum . map (uncurry (+) . join bimap (maxPressureRelease matrix 26 start)) $ allSplits valves
  where
    matrix = distanceMatrix (bounds graph) (graph !)
    Just start = vertexFromKey "AA"
    vertices   = indices graph
    valves     = Map.fromList [(v, fr) | v <- vertices, let (fr, _, _) = nodeFromVertex v, fr > 0]

allSplits :: Map.Map Vertex Int -> [(Map.Map Vertex Int, Map.Map Vertex Int)]
allSplits m = [(x, m `Map.difference` x) | x <- allSubmaps m]

allSubmaps :: Ord a => Map.Map a b -> [Map.Map a b]
allSubmaps m
    | Map.null m = [m]
    | otherwise  = restMaps ++ map (Map.insert k e) restMaps
  where
    (k, e)   = Map.elemAt 0 m
    restMaps = allSubmaps $ Map.delete k m

main :: IO ()
main = do
    args <- getArgs
    let hasElephant = not $ null args
    print . solve hasElephant . loadGraph =<< getContents
