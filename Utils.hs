module Utils (parseBlock, parseBlocks, parseGroups, splitPair) where

import Data.List.Split (splitOn, chunksOf)

parseBlock :: (String -> a) -> String -> [a]
parseBlock parseLine = map parseLine . lines

parseBlocks :: (String -> a) -> String -> [[a]]
parseBlocks parseLine = map (parseBlock parseLine) . splitOn "\n\n"

parseGroups :: Int -> (String -> a) -> String -> [[a]]
parseGroups n parseLine = chunksOf n . parseBlock parseLine

splitPair :: String -> (String, String)
splitPair = (\ [x,y] -> (x, y)) . words
