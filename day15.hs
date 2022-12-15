#!/usr/bin/env runhaskell

import Data.Either.Combinators (fromRight')
import Data.List (find)
import System.Environment (getArgs)
import Text.Parsec (Parsec, endBy, many1, parse)
import Text.Parsec.Char (newline, oneOf, string)

-- Types and constructors / destructors
data Pos = Pos
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord, Show)

newtype Beacon = Beacon { bPos :: Pos } 
    deriving (Eq, Show)

data Sensor = Sensor
    { sPos :: Pos
    , nearestBeacon :: Beacon
    , beaconDist :: Int
    } deriving (Eq, Show)

makeSensor :: Pos -> Beacon -> Sensor
makeSensor sp beacon = Sensor sp beacon (distance sp (bPos beacon))

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
    makeSensor sensorPos . Beacon <$> posParser

inputParser :: Parsec String () [Sensor]
inputParser = endBy sensorParser newline

-- Utility
distance :: Pos -> Pos -> Int
distance (Pos x1 y1) (Pos x2 y2) = abs (x2 - x1) + abs (y2 - y1)

outOfRange :: Pos -> Sensor -> Bool
outOfRange newPos sensor@(Sensor sp _ d) = distance newPos sp > d

-- Part 1 solution
validForSensor :: Pos -> Sensor -> Bool
validForSensor newPos sensor@(Sensor _ (Beacon bp) _) = newPos == bp || outOfRange newPos sensor

part1 :: Int -> [Sensor] -> Int
part1 yMax sensors = sum . map (fromEnum . (\pos -> not . all (validForSensor pos) $ sensors)) $ candidates
  where
    y          = yMax `div` 2
    x1         = minimum [ x - d | (Sensor (Pos x y) _ d) <- sensors]
    x2         = maximum [ x + d | (Sensor (Pos x y) _ d) <- sensors]
    candidates = [Pos x y | x <- [x1 .. x2]]

-- Part 2 solution
fringe :: Sensor -> [Pos]
fringe sensor@(Sensor (Pos x y) _ d) =
    zipWith Pos [x-d-1..x] [y..y+d+1] ++
    zipWith Pos [x-d-1..x] [y,y-1..y-d-1] ++
    zipWith Pos [x..x+d+1] [y-d-1..y] ++
    zipWith Pos [x..x+d+1] [y+d+1,y+d..y]

inBounds :: (Int, Int) -> Pos -> Bool
inBounds (minVal, maxVal) (Pos x y) = x >= minVal && x <= maxVal && y >= minVal && y <= maxVal

part2 :: Int -> [Sensor] -> Int
part2 yMax sensors = 4000000 * x + y
  where
    candidates     = filter (inBounds (0, yMax)) . concatMap fringe $ sensors
    Just (Pos x y) = find (\pos -> all (outOfRange pos) sensors) candidates

main :: IO ()
main = do
    args <- getArgs
    let handler = if null args || (not . read . head) args then part1 else part2
    let yMax = if length args < 2 then 4000000 else read (args !! 1)
    getContents >>= print . handler yMax . fromRight' . parse inputParser ""
