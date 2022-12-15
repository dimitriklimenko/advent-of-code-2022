#!/usr/bin/env runhaskell

import Data.Either.Combinators (fromRight')
import Data.List (intercalate)
import Data.List.HT (mapAdjacent)
import qualified Data.Map as Map (Map, fromList, insert, keys, lookup)
import Data.Maybe (fromMaybe)
import GHC.Utils.Misc (last2)
import System.Environment (getArgs)
import Text.Parsec (Parsec, endBy, many1, parse, sepBy)
import Text.Parsec.Char (char, digit, newline, string)

-- Core data types
type Pos = (Int, Int)

data Tile = Empty | Rock | Sand
    deriving (Eq)

data Cave = Cave
    { tileMap :: Map.Map Pos Tile
    , yMax :: Int
    , hasFloor :: Bool
    }

-- Input data types and parsing
number :: Parsec String () Int
number = read <$> many1 digit

posParser :: Parsec String () Pos
posParser = last2 <$> sepBy number (char ',')

pathParser :: Parsec String () [Pos]
pathParser = sepBy posParser (string " -> ")

inputParser :: Parsec String () [[Pos]]
inputParser = endBy pathParser newline

-- Displaying the cave
toChar :: Tile -> Char
toChar Empty = '.'
toChar Rock  = '#'
toChar Sand  = 'o'

instance Show Tile where
    show :: Tile -> String
    show tile = [toChar tile]

getTile :: Pos -> Cave -> Tile
getTile pos@(x, y) (Cave tileMap yMax hasFloor)
    | hasFloor && y >= yMax = Rock
    | otherwise             = fromMaybe Empty . Map.lookup pos $ tileMap

isEmpty :: Cave -> Pos -> Bool
isEmpty cave pos = getTile pos cave == Empty

instance Show Cave where
    show :: Cave -> String
    show cave = intercalate "\n" [[toChar $ getTile (x, y) cave | x <- [x1..x2]] | y <- [y1..y2]]
      where
        (x1, x2, y1, y2) = bounds cave

-- Building the cave
makeLine :: Pos -> Pos -> [Pos]
makeLine (x1, y1) (x2, y2)
    | x1 == x2 = [(x1, y) | y <- [y1, y1 + signum (y2 - y1)..y2]]
    | y1 == y2 = [(x, y1) | x <- [x1, x1 + signum (x2 - x1)..x2]]

buildCave :: Bool -> [[Pos]] -> Cave
buildCave hasFloor paths = Cave (Map.fromList entries) yMax hasFloor
  where
    entries :: [(Pos, Tile)]
    entries = (`zip` repeat Rock) . concatMap (concat . mapAdjacent makeLine) $ paths
    yMax    = (if hasFloor then 2 else 0) + (maximum . map (snd . fst) $ entries)

bounds :: Cave -> (Int, Int, Int, Int)
bounds (Cave tileMap yMax _) = (x1, x2, y1, yMax)
  where
    x1 = minimum . map fst . Map.keys $ tileMap
    x2 = maximum . map fst . Map.keys $ tileMap
    y1 = minimum . map snd . Map.keys $ tileMap

-- Dropping sand
getRestPos :: Cave -> Pos -> Maybe Pos
getRestPos cave pos@(x, y)
    | not (isEmpty cave pos)  = Nothing
    | y >= yMax cave          = Nothing
    | isEmpty cave (x, y+1)   = getRestPos cave (x, y+1)
    | isEmpty cave (x-1, y+1) = getRestPos cave (x-1, y+1)
    | isEmpty cave (x+1, y+1) = getRestPos cave (x+1, y+1)
    | otherwise               = Just pos

dropSand :: Cave -> (Cave, Bool)
dropSand cave@(Cave tileMap yMax hasFloor) = case getRestPos cave (500, 0) of
    Nothing  -> (cave, False)
    Just pos -> (Cave (Map.insert pos Sand tileMap) yMax hasFloor, True)

simulate :: Cave -> (Cave, Int)
simulate cave = if cameToRest then (newCave', numSteps + 1) else (newCave, 0)
  where
    (newCave,  cameToRest) = dropSand cave
    (newCave', numSteps)   = simulate newCave

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . snd. simulate . buildCave (not $ null args) . fromRight' . parse inputParser ""
