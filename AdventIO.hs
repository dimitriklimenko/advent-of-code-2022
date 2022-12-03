module AdventIO (parseBlock, parseBlocks) where

import Data.List.Split

parseBlock :: (Read a) => (String -> a) -> String -> [a]
parseBlock parseLine = map parseLine . lines

parseBlocks :: (Read a) => (String -> a) -> String -> [[a]]
parseBlocks parseLine = map (parseBlock parseLine) . splitOn "\n\n"
