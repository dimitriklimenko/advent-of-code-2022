#!/usr/bin/env -S runhaskell -i..

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

import AdventIO3 (readListListInts)

day1 :: [[Integer]] -> Integer
day1 = sum . take 3 . sortBy (comparing Down) . map sum

main :: IO ()
main = readListListInts >>= print . day1
