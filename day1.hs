#!/usr/bin/env runhaskell

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import System.Environment (getArgs)

import Utils (parseBlocks)

day1 :: Int -> [[Integer]] -> Integer
day1 n = sum . take n . sortBy (comparing Down) . map sum

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . day1 (if null args then 1 else 3) . parseBlocks read
