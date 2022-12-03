#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import Data.Set (Set, fromList, intersection, size, toList)

import Utils (parseBlock)

overlap :: String -> Set Char
overlap rucksack =
    let (a, b) = splitAt (length rucksack `div` 2) rucksack
    in fromList a `intersection` fromList b

single :: Set Char -> Char
single x
    | size x == 1 = head $ toList x
    | otherwise   = error "wrong # of duplicate items"

priority :: Char -> Int
priority item
    | v >= 97   = v - 96
    | otherwise = v - 38
    where v = fromEnum item

day3 :: [String] -> Int
day3 = sum . map (priority . single . overlap)

main :: IO ()
main = do
    -- args <- getArgs
    -- let parsePair = if null args then parsePart1 else parsePart2
    getContents >>= print . day3 . parseBlock id
