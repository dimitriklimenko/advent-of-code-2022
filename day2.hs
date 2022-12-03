#!/usr/bin/env runhaskell

import Data.List.Split

import AdventIO (parseBlock)

data Move = Rock | Paper | Scissors
    deriving (Eq, Read, Show)

readYou :: String -> Move
readYou "X" = Rock
readYou "Y" = Paper
readYou "Z" = Scissors

readOpponent :: String -> Move
readOpponent "A" = Rock
readOpponent "B" = Paper
readOpponent "C" = Scissors

parseWords :: [String] -> (Move, Move)
parseWords [a, b] = (readOpponent a, readYou b)

shapeScore :: Move -> Integer
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

counter :: Move -> Move
counter Rock     = Paper
counter Paper    = Scissors
counter Scissors = Rock

outcomeScore :: (Move, Move) -> Integer
outcomeScore (opp, you)
    | you == counter opp    = 6
    | you == opp            = 3
    | otherwise             = 0

scoreRound :: (Move, Move) -> Integer
scoreRound (opp, you) = shapeScore you + outcomeScore (opp, you)

day2 :: [(Move, Move)] -> Integer
day2 = sum . map scoreRound

main :: IO ()
main = getContents >>= print . day2 . parseBlock (parseWords . words)
