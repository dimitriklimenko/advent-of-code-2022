module Utils (parseBlock, parseBlocks, splitPair) where

import Data.List.Split

parseBlock :: (String -> a) -> String -> [a]
parseBlock parseLine = map parseLine . lines

parseBlocks :: (String -> a) -> String -> [[a]]
parseBlocks parseLine = map (parseBlock parseLine) . splitOn "\n\n"

splitPair :: String -> (String, String)
splitPair = (\ [x,y] -> (x, y)) . words
