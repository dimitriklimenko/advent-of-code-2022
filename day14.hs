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
    , xMin :: Int
    , xMax :: Int
    , yMin :: Int
    , yMax :: Int
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

getTile :: Pos -> Map.Map Pos Tile -> Tile
getTile pos = fromMaybe Empty . Map.lookup pos

instance Show Cave where
    show :: Cave -> String
    show cave = intercalate "\n" [[toChar $ getTile (x, y) (tileMap cave) | x <- [xMin cave .. xMax cave]] | y <- [yMin cave .. yMax cave]]

-- Building the cave
makeLine :: Pos -> Pos -> [Pos]
makeLine (x1, y1) (x2, y2)
    | x1 == x2 = [(x1, y) | y <- [y1, y1 + signum (y2 - y1)..y2]]
    | y1 == y2 = [(x, y1) | x <- [x1, x1 + signum (x2 - x1)..x2]]

buildCave :: [[Pos]] -> Cave
buildCave posns = Cave (Map.fromList entries) x1 x2 y1 y2
  where
    entries :: [(Pos, Tile)]
    entries = (((500, 0), Empty) :) . (`zip` repeat Rock) . concatMap (concat . mapAdjacent makeLine) $ posns
    x1      = minimum . map (fst . fst) $ entries
    x2      = maximum . map (fst . fst) $ entries
    y1      = minimum . map (snd . fst) $ entries
    y2      = maximum . map (snd . fst) $ entries

-- Bounds checking and dropping sand
isEmpty :: Cave -> Pos -> Bool
isEmpty (Cave tileMap _ _ _ _) pos = getTile pos tileMap == Empty

inBounds :: Cave -> Pos -> Bool
inBounds (Cave _ x1 x2 y1 y2) (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

getRestPos :: Cave -> Pos -> Maybe Pos
getRestPos cave pos@(x, y)
    | not (inBounds cave pos) = Nothing
    | isEmpty cave (x, y+1)   = getRestPos cave (x, y+1)
    | isEmpty cave (x-1, y+1) = getRestPos cave (x-1, y+1)
    | isEmpty cave (x+1, y+1) = getRestPos cave (x+1, y+1)
    | otherwise               = Just pos

dropSand :: Cave -> (Cave, Bool)
dropSand cave@(Cave tileMap x1 x2 y1 y2) = case getRestPos cave (500, 0) of
    Nothing  -> (cave, False)
    Just pos -> (Cave (Map.insert pos Sand tileMap) x1 x2 y1 y2, True)

simulate :: Cave -> Int
simulate cave = if cameToRest then 1 + simulate newCave else 0
  where
    (newCave, cameToRest) = dropSand cave

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . simulate . buildCave . fromRight' . parse inputParser ""
