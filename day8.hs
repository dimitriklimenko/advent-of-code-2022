#!/usr/bin/env runhaskell

import Data.Map (empty, foldrWithKey, insert)
import Data.Char (digitToInt)
import Data.List (transpose, zipWith4)
import System.Environment (getArgs)

visibleRow :: [Int] -> [Bool]
visibleRow = helper (-1)
    where helper h []     = []
          helper h (x:xs) = (x > h):helper (max h x) xs

numSeenRow :: [Int] -> [Int]
numSeenRow = helper empty 0
    where helper m _   []     = []
          helper m pos (x:xs) = (pos - foldrWithKey
                (\h hPos blockPos -> max blockPos (if h >= x then hPos else 0))
                0 m) : helper (insert x pos m) (pos+1) xs

combineDirections :: ([Int] -> [a]) -> (a -> a -> a) -> [[Int]] -> [[a]]
combineDirections rowFunc (??) rows =
    let right = map rowFunc rows
        left = map (reverse . rowFunc . reverse) rows
        down = transpose . map rowFunc . transpose $ rows
        up = transpose . map (reverse . rowFunc . reverse) . transpose $ rows
    in zipWith4 (zipWith4 (\a b c d -> a ?? b ?? c ?? d)) right left down up

part1 :: [[Int]] -> Int
part1 = sum . map (sum . map fromEnum) . combineDirections visibleRow (||)

part2 :: [[Int]] -> Int
part2 = maximum . map maximum . combineDirections numSeenRow (*)

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . (if null args then part1 else part2) . map (map digitToInt) . lines
