#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import Data.Array (array, Array, listArray, elems, (//), (!))
import Data.Bifunctor (first, bimap)
import Data.Char (isSpace)
import Data.List (transpose, dropWhile)
import Data.List.Split (chunksOf)
import Data.Maybe (Maybe)

import Utils (parseBlock, splitPairOn, mapPair)

newtype Action = Action (Int, Int, Int)
    deriving Show

type State = Array Int String

makeOneBasedArray :: [a] -> Array Int a
makeOneBasedArray xs = listArray (1, length xs) xs

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseRow :: String -> String
parseRow = map (!! 1) . chunksOf 4

parseState :: String -> State
parseState = makeOneBasedArray . map trimLeft . transpose . init . parseBlock parseRow

parseAction :: String -> Action
parseAction s = Action (read $ wds !! 1, read $ wds !! 3, read $ wds !! 5)
    where wds = words s

moveCrate :: Int -> Int -> State -> State
moveCrate from to s = s // [(from, fs), (to, f:ts)]
    where (f:fs) = s ! from
          ts     = s ! to

processActions :: (State, [Action]) -> (State, [Action])
processActions (s, [])                      = (s, [])
processActions (s, Action (0, from, to):xs) = processActions (s, xs)
processActions (s, Action (n, from, to):xs) = processActions (moveCrate from to s, Action (n-1, from, to):xs)

day5 :: (State, [Action]) -> String
day5 = map head . elems . fst . processActions

main :: IO ()
main = do
    args <- getArgs
    getContents >>= putStrLn . day5 . bimap parseState (parseBlock parseAction) . splitPairOn "\n\n"
