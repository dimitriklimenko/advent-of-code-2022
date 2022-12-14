#!/usr/bin/env runhaskell

import Data.Char (ord, chr, isLower)
import Data.Either.Combinators (fromRight')
import Data.Set (Set, empty, fromList, singleton, union)
import Data.Map (Map, empty, insert, member, notMember, (!))
import Data.Tuple.Extra (uncurry3)
import GHC.Utils.Misc (ordNub)
import System.Environment (getArgs)
import Text.Parsec (Parsec, char, parse, try, (<|>))
import Data.List (genericDrop)

type Pos = (Int, Int)

data Square = Start {height :: Int} | End {height :: Int} | Other {height :: Int}
    deriving (Show)

type HeightMap = Map Pos Int
type DistMap = Map Pos Int

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
parseGrid = makeGrid . foldr combiner (Data.Map.empty, Nothing, Nothing) . flattenWithIndices . map (map parseSquare) . lines
  where
    makeGrid :: (HeightMap, Maybe Pos, Maybe Pos) -> Grid
    makeGrid (heights, Just s, Just e) = Grid heights s e

    addEntry :: (Pos, Square) -> HeightMap -> HeightMap
    addEntry (p, s) = insert p (height s)

    combiner :: (Pos, Square) -> (HeightMap, Maybe Pos, Maybe Pos) -> (HeightMap, Maybe Pos, Maybe Pos)
    combiner cell@(p, Start _) (m, _, e) = (addEntry cell m, Just p, e)
    combiner cell@(p, End _)   (m, s, _) = (addEntry cell m, s,      Just p)
    combiner cell@(p, Other _) (m, s, e) = (addEntry cell m, s,      e)

neighbours :: Pos -> [Pos]
neighbours (i, j) = [(i-1, j), (i, j-1), (i, j+1), (i+1, j)]

nexts :: HeightMap -> Pos -> [Pos]
nexts hMap p = [p2 | let h1 = hMap ! p, p2 <- neighbours p, member p2 hMap, let h2 = hMap ! p2, h2 - h1 <= 1]

stepDist :: HeightMap -> Int -> (DistMap, Set Pos) -> (DistMap, Set Pos)
stepDist hMap dist (m, ps) = foldr (\p1 (m1, pp) -> (
    insert p1 dist m1, pp `union` Data.Set.fromList [p | p <- nexts hMap p1, notMember p m])) (m, Data.Set.empty) ps

getDistance :: Grid -> Int
getDistance grid = helper grid 0 (Data.Map.empty, singleton $ startPos grid)
  where
    helper :: Grid -> Int -> (DistMap, Set Pos) -> Int
    helper grid@(Grid hMap _ end) currentDist state@(dMap, nextPosns)
      | member end dMap = dMap ! end
      | otherwise       = helper grid (currentDist + 1) (stepDist hMap currentDist state)

main :: IO ()
main = do
    args <- getArgs
    grid <- parseGrid <$> getContents
    let x = getDistance grid
    print x
