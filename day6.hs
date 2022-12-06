#!/usr/bin/env runhaskell

import Data.List (dropWhile, nub, splitAt)
import System.Environment (getArgs)

getMarkerPos :: Int -> String -> Int
getMarkerPos markerLength lst
    | length (nub front) == markerLength = markerLength
    | otherwise                          = 1 + getMarkerPos markerLength (tail lst)
    where (front, back) = splitAt markerLength lst

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . getMarkerPos (if null args then 4 else 14)
