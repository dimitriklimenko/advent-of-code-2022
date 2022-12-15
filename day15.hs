#!/usr/bin/env runhaskell

import Data.Either.Combinators (fromRight')
import Data.List (find, unzip4)
import System.Environment (getArgs)
import Text.Parsec (Parsec, endBy, many1, parse)
import Text.Parsec.Char (newline, oneOf, string)

-- Types
data Pos = Pos
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord, Show)

newtype Beacon = Beacon { bPos :: Pos } 
    deriving (Eq, Show)

data Sensor = Sensor
    { sPos :: Pos
    , nearestBeacon :: Beacon
    } deriving (Eq, Show)

-- Parsing
number :: Parsec String () Int
number = read <$> many1 (oneOf $ '-':['0'..'9'])

posParser :: Parsec String () Pos
posParser = string "x=" >> Pos <$> number <* string ", y=" <*> number

sensorParser :: Parsec String () Sensor
sensorParser = do
    string "Sensor at "
    sensorPos <- posParser
    string ": closest beacon is at "
    Sensor sensorPos . Beacon <$> posParser

inputParser :: Parsec String () [Sensor]
inputParser = endBy sensorParser newline

-- Utility
distance :: Pos -> Pos -> Int
distance (Pos x1 y1) (Pos x2 y2) = abs (x2 - x1) + abs (y2 - y1)

beaconDist :: Sensor -> Int
beaconDist (Sensor sp (Beacon bp)) = distance sp bp

distBounds :: Int -> Sensor -> (Int, Int, Int, Int)
distBounds d (Sensor (Pos x y) _) = (x-d, x+d, y-d, y+d)

combinedBounds :: [Sensor] -> (Int, Int, Int, Int)
combinedBounds sensors = (minimum x1s, maximum x2s, minimum y1s, maximum y2s)
  where
    (x1s, x2s, y1s, y2s) = unzip4 . map (\s -> distBounds (beaconDist s) s) $ sensors

outOfRange :: Pos -> Sensor -> Bool
outOfRange newPos sensor@(Sensor sp _) = distance newPos sp > beaconDist sensor

validForSensor :: Pos -> Sensor -> Bool
validForSensor newPos sensor@(Sensor _ (Beacon bp)) = newPos == bp || outOfRange newPos sensor

part1 :: Int -> [Sensor] -> Int
part1 yMax sensors = sum . map fromEnum $ [not $ all (validForSensor (Pos x (yMax `div` 2))) sensors | x <- [x1..x2]]
  where
    (x1, x2, _, _) = combinedBounds sensors

fringe :: Sensor -> [Pos]
fringe sensor@(Sensor (Pos x y) _) =
    zipWith Pos [x1..x] [y..y2] ++
    zipWith Pos [x1..x] [y,y-1..y1] ++
    zipWith Pos [x..x2] [y1..y] ++
    zipWith Pos [x..x2] [y2,y2-1..y]
  where
    d                = beaconDist sensor
    (x1, x2, y1, y2) = distBounds (d+1) sensor

inBounds :: (Int, Int) -> Pos -> Bool
inBounds (minVal, maxVal) (Pos x y) = x >= minVal && x <= maxVal && y >= minVal && y <= maxVal

part2 :: Int -> [Sensor] -> Int
part2 yMax sensors = 4000000 * x + y
  where
    candidates     = filter (inBounds (0, yMax)) . concatMap fringe $ sensors
    Just (Pos x y) = find (\x -> all (outOfRange x) sensors) candidates

main :: IO ()
main = do
    args <- getArgs
    let handler = if null args || (not . read . head) args then part1 else part2
    let yMax = if length args < 2 then 4000000 else read (args !! 1)
    getContents >>= print . handler yMax . fromRight' . parse inputParser ""

