#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import Data.Array (Array, listArray, elems, (//), (!))
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.List (transpose, dropWhile, splitAt, foldl')
import Data.List.Split (chunksOf)

import Utils (parseBlock, splitPairOn)

type Action = (Int, Int, Int)
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
parseAction s = (read $ wds !! 1, read $ wds !! 3, read $ wds !! 5)
    where wds = words s

moveCrate :: (Int, Int) -> State -> State
moveCrate (from, to) state = state // [(from, fs), (to, f:ts)]
    where (f:fs) = state ! from
          ts     = state ! to

moveCrates1 :: Action -> State -> State
moveCrates1 (0, from, to) = id
moveCrates1 (n, from, to) = moveCrates1 (n-1, from, to) . moveCrate (from, to)

moveCrates2 :: Action -> State -> State
moveCrates2 (n, from, to) state = state // [(from, f2), (to, f1 ++ ts)]
    where (f1, f2) = splitAt n $ state ! from
          ts       = state ! to

day5 :: (Action -> State -> State) -> (State, [Action]) -> String
day5 moveFunc (state, actions) = map head . elems $ foldl' (flip moveFunc) state actions

main :: IO ()
main = do
    args <- getArgs
    let moveFunc = if null args then moveCrates1 else moveCrates2
    getContents >>= putStrLn . day5 moveFunc . bimap parseState (parseBlock parseAction) . splitPairOn "\n\n"
