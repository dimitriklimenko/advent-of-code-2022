#!/usr/bin/env runhaskell

import System.Environment (getArgs)

import Utils (parseBlock, splitPair)


data Shape = Rock | Paper | Scissors
    deriving (Eq, Show)

readShape :: String -> Shape
readShape "A" = Rock
readShape "B" = Paper
readShape "C" = Scissors
readShape "X" = Rock
readShape "Y" = Paper
readShape "Z" = Scissors

shapeScore :: Shape -> Integer
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3


data Outcome = Loss | Draw | Win
    deriving (Eq, Show)

readOutcome :: String -> Outcome
readOutcome "X" = Loss
readOutcome "Y" = Draw
readOutcome "Z" = Win

shapeForOutcome :: Outcome -> Shape -> Shape
shapeForOutcome Win   Rock      = Paper
shapeForOutcome Win   Paper     = Scissors
shapeForOutcome Win   Scissors  = Rock
shapeForOutcome Draw  opp       = opp
shapeForOutcome Loss  Rock      = Scissors
shapeForOutcome Loss  Paper     = Rock
shapeForOutcome Loss  Scissors  = Paper

outcome :: (Shape, Shape) -> Outcome
outcome (opp, you)
    | you == shapeForOutcome Win opp    = Win
    | you == opp                        = Draw
    | otherwise                         = Loss

outcomeScore :: Outcome -> Integer
outcomeScore Loss = 0
outcomeScore Draw = 3
outcomeScore Win  = 6


scoreRound :: (Shape, Shape) -> Integer
scoreRound (opp, you) = shapeScore you + outcomeScore (outcome (opp, you))

parsePart1 :: (String, String) -> (Shape, Shape)
parsePart1 (a, b) = (readShape a, readShape b)

parsePart2 :: (String, String) -> (Shape, Shape)
parsePart2 (a, b) =
    let opp = readShape a
        outcome = readOutcome b
    in (opp, shapeForOutcome outcome opp)

day2 :: ((String, String) -> (Shape, Shape)) -> [(String, String)] -> Integer
day2 parsePair = sum . map (scoreRound . parsePair)

main :: IO ()
main = do
    args <- getArgs
    let parsePair = if null args then parsePart1 else parsePart2
    getContents >>= print . day2 parsePair . parseBlock splitPair
