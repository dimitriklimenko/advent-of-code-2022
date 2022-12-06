#!/usr/bin/env runhaskell

import Data.Array (Ix, Array, array, (!), (//))
import Data.Maybe (Maybe)
import System.Environment (getArgs)

type CharCounts = (Array Char Int, Int)

emptyCharCounts :: CharCounts
emptyCharCounts = (array ('a', 'z') [(c, 0) | c <- ['a'..'z']], 0)

updateCount :: Int -> Char -> CharCounts -> CharCounts
updateCount 0     _ cc                  = cc
updateCount delta c (counts, nDistinct) = 
    let oldCount = counts ! c
        newCount = oldCount + delta
    in (counts // [(c, newCount)], case (oldCount, newCount) of
        (0, _) -> nDistinct + 1
        (_, 0) -> nDistinct - 1
        _      -> nDistinct)

addChar :: Char -> CharCounts -> CharCounts
addChar = updateCount 1

removeChar :: Char -> CharCounts -> CharCounts
removeChar = updateCount (-1)

initCharCounts :: String -> CharCounts
initCharCounts = foldr addChar emptyCharCounts

getMarkerPos :: Int -> String -> Int
getMarkerPos markerLength buffer = getMarkerPosHelper markerLength markerLength (zip back buffer) (initCharCounts front)
    where (front, back) = splitAt markerLength buffer

getMarkerPosHelper :: Int -> Int -> [(Char, Char)] -> CharCounts -> Int
getMarkerPosHelper markerLength position inOutList cc@(_, nDistinct)
    | nDistinct == markerLength = position
    | null inOutList            = 0
    | otherwise                 = let (new, old):rest = inOutList in
        getMarkerPosHelper markerLength (position + 1) rest (removeChar old . addChar new $ cc)

main :: IO ()
main = do
    args <- getArgs
    let markerLength = (if null args then 4 else read (head args))
    getLine >>= print . getMarkerPos markerLength
