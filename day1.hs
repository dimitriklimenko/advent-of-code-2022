#!/usr/bin/env runhaskell

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

import AdventIO (parseBlocks)

day1 :: [[Integer]] -> Integer
day1 = sum . take 3 . sortBy (comparing Down) . map sum

main :: IO ()
main = getContents >>= print . day1 . parseBlocks read
