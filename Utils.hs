module Utils (mapPair, parseBlock, parseBlocks, parseGroups, splitPairOn, tuplify2) where

import Data.List.Split (splitOn, chunksOf)

parseBlock :: (String -> a) -> String -> [a]
parseBlock parseLine = map parseLine . lines

parseBlocks :: (String -> a) -> String -> [[a]]
parseBlocks parseLine = map (parseBlock parseLine) . splitOn "\n\n"

parseGroups :: Int -> (String -> a) -> String -> [[a]]
parseGroups n parseLine = chunksOf n . parseBlock parseLine

tuplify2 :: (Show a) => [a] -> (a, a)
tuplify2 (x:y:_) = (x, y)
tuplify2 lst     = error $ "Unexpected list size" ++ show lst

splitPairOn :: String -> String -> (String, String)
splitPairOn separator = tuplify2 . splitOn separator

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a1, a2) = (f a1, f a2)
