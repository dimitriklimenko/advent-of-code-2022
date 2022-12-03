#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import Data.CharSet (CharSet, empty, fromList, intersection, size, toList)

import Utils (parseBlock, parseGroups)

overlap :: [CharSet] -> CharSet
overlap []     = empty
overlap [x]    = x
overlap (x:xs) = x `intersection` overlap xs

part1Overlap :: String -> CharSet
part1Overlap rucksack = fromList a `intersection` fromList b
    where (a, b) = splitAt (length rucksack `div` 2) rucksack

part2Overlap :: [String] -> CharSet
part2Overlap = overlap . map fromList

single :: CharSet -> Char
single x
    | size x == 1 = head $ toList x
    | otherwise   = error "wrong # of duplicate items"

priority :: Char -> Int
priority item
    | v >= 97   = v - 96
    | otherwise = v - 38
    where v = fromEnum item

day3Part1 :: [String] -> Int
day3Part1 = sum . map (priority . single . part1Overlap)

day3Part2 :: [[String]] -> Int
day3Part2 = sum . map (priority . single . part2Overlap)

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . if null args
        then day3Part1 . parseBlock id
        else day3Part2 . parseGroups 3 id
