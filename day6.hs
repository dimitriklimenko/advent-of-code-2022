#!/usr/bin/env runhaskell

import GHC.Utils.Misc (ordNub)
import System.Environment (getArgs)

getMarkerPos :: Int -> String -> Int
getMarkerPos markerLength lst
    | (length . ordNub . take markerLength) lst == markerLength = markerLength
    | otherwise                                                 = 1 + getMarkerPos markerLength (tail lst)

main :: IO ()
main = do
    args <- getArgs
    let markerLength = (if null args then 4 else read (head args))
    getLine >>= print . getMarkerPos markerLength
