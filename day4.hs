#!/usr/bin/env runhaskell

import System.Environment (getArgs)

import Utils (parseBlock, splitPairOn, mapTuple)

type Assignment = (Int, Int)

readAssignment :: String -> Assignment
readAssignment = mapTuple read . splitPairOn "-"

parseLine :: String -> (Assignment, Assignment)
parseLine = mapTuple readAssignment . splitPairOn ","

hasFullOverlap :: (Assignment, Assignment) -> Bool
hasFullOverlap ((lo1, hi1), (lo2, hi2)) =
    (lo1 <= lo2 && hi1 >= hi2) || (lo2 <= lo1 && hi2 >= hi1)

hasPartialOverlap :: (Assignment, Assignment) -> Bool
hasPartialOverlap ((lo1, hi1), (lo2, hi2)) =
    (lo1 <= hi2 && hi1 >= lo2) || (lo2 <= hi1 && hi2 >= lo1)

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . length . filter (if null args then hasFullOverlap else hasPartialOverlap) . parseBlock parseLine
