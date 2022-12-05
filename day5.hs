#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import Data.Array (array, Array, listArray, elems, (//), (!))
import Data.Bifunctor (first, bimap)
import Data.Char (isSpace)
import Data.List (transpose, dropWhile, splitAt, foldl')
import Data.List.Split (chunksOf)
import Data.Maybe (Maybe)

import Utils (parseBlock, splitPairOn, mapPair)

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

moveCrates1 :: State -> Action -> State
moveCrates1 state (0, from, to) = state
moveCrates1 state (n, from, to) = moveCrates1 (moveCrate (from, to) state) (n-1, from, to) 

moveCrates2 :: State -> Action -> State
moveCrates2 state (n, from, to) = state // [(from, f2), (to, f1 ++ ts)]
    where (f1, f2) = splitAt n $ state ! from
          ts       = state ! to

processActions :: (State -> Action -> State) -> (State, [Action]) -> State
processActions moveFunc (state, actions) = foldl' moveFunc state actions

day5 :: (State -> Action -> State) -> (State, [Action]) -> String
day5 moveFunc = map head . elems . processActions moveFunc

main :: IO ()
main = do
    args <- getArgs
    let moveFunc = (if null args then moveCrates1 else moveCrates2)
    getContents >>= putStrLn . day5 moveFunc . bimap parseState (parseBlock parseAction) . splitPairOn "\n\n"
