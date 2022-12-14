#!/usr/bin/env runhaskell

import Data.Char (ord, chr, isLower)
import qualified Data.Set as Set (Set, empty, fromList, singleton, union)
import qualified Data.Map as Map (Map, empty, insert, member, notMember)
import Data.Map ((!))
import System.Environment (getArgs)

type Pos = (Int, Int)

data Square = Start {height :: Int} | End {height :: Int} | Other {height :: Int}
    deriving (Show)

type HeightMap = Map.Map Pos Int
type DistMap = Map.Map Pos Int

data Grid = Grid
    { heightMap :: HeightMap
    , startPos :: Pos
    , endPos :: Pos
    } deriving (Show)

parseSquare :: Char -> Square
parseSquare c
    | isLower c = Other $ ord c - ord 'a'
    | c == 'S'  = Start 0
    | c == 'E'  = End 25

flattenWithIndices :: [[a]] -> [((Int, Int), a)]
flattenWithIndices xss = concat [[((i, j), s) | (j, s) <- zip [0..] row] | (i, row) <- zip [0..] xss]

parseGrid :: String -> Grid
parseGrid = makeGrid . foldr combine (Map.empty, Nothing, Nothing) . flattenWithIndices . map (map parseSquare) . lines
  where
    makeGrid :: (HeightMap, Maybe Pos, Maybe Pos) -> Grid
    makeGrid (heights, Just start, Just end) = Grid heights start end

    combine :: (Pos, Square) -> (HeightMap, Maybe Pos, Maybe Pos) -> (HeightMap, Maybe Pos, Maybe Pos)
    combine cell@(p, Start h) (m, _, e) = (Map.insert p h m, Just p, e)
    combine cell@(p, End h)   (m, s, _) = (Map.insert p h m, s,      Just p)
    combine cell@(p, Other h) (m, s, e) = (Map.insert p h m, s,      e)

neighbours :: Pos -> [Pos]
neighbours (i, j) = [(i-1, j), (i, j-1), (i, j+1), (i+1, j)]

-- Adjacent squares within a reachable height
nexts :: HeightMap -> Pos -> [Pos]
nexts hMap p = [p' | let h = hMap ! p, p' <- neighbours p, Map.member p' hMap, let h' = hMap ! p', h - h' <= 1]

-- Single step update to DistMap. Takes a height map, current distance, and a pair consisting of the current DistMap
--  and a set of positions to add at the current distance. Returns an updated DistMap and the next set of positions.
stepDistMap :: HeightMap -> Int -> (DistMap, Set.Set Pos) -> (DistMap, Set.Set Pos)
stepDistMap hMap dist (dMap, ps) = foldr combine (dMap, Set.empty) ps
  where
    combine :: Pos -> (DistMap, Set.Set Pos) -> (DistMap, Set.Set Pos)
    combine p (dMap, nextPs) = (Map.insert p dist dMap, nextPs `Set.union` Set.fromList [p' | p' <- nexts hMap p, Map.notMember p' dMap])

-- Recursive solution. Starts at the end cell of the grid, and takes a test for whether or not a cell is a valid start cell.
getDistance :: (Pos -> Bool) -> Grid -> Int
getDistance test grid = helper grid 0 (Map.empty, Set.singleton $ endPos grid)
  where
    helper :: Grid -> Int -> (DistMap, Set.Set Pos) -> Int
    helper grid@(Grid hMap start _) currentDist state@(dMap, nextPosns)
      | any test nextPosns = currentDist
      | otherwise          = helper grid (currentDist + 1) (stepDistMap hMap currentDist state)

main :: IO ()
main = do
    args <- getArgs
    grid <- parseGrid <$> getContents
    let startTest = if null args then (== startPos grid) else (\x -> heightMap grid ! x == 0)
    print . getDistance startTest $ grid
