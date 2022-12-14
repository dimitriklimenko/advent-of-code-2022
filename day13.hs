#!/usr/bin/env runhaskell

import Data.List (findIndices, sort)
import Data.List.Split (chunksOf, splitOn)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadPrec (ReadPrec, choice)
import Text.Read (Read, readPrec)

data Packet = Val Int | List [Packet]
  deriving (Eq)

instance Show Packet where
  show :: Packet -> String
  show (Val x)   = show x
  show (List ps) = show ps

instance Read Packet where
  readPrec :: ReadPrec Packet
  readPrec = choice [fmap Val readPrec, fmap List readPrec]

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Val x1)   (Val x2)   = compare x1 x2
  compare (List ps1) (List ps2) = compare ps1 ps2
  compare (Val x1)   (List ps2) = compare (List [Val x1]) (List ps2)
  compare (List ps1) (Val x2)   = compare (List ps1) (List [Val x2])

parseList :: String -> [Packet]
parseList = map read . filter (not . null) . lines

part1 :: [Packet] -> Int
part1 packets = sum [i | (i, [p1, p2]) <- zip [1..] (chunksOf 2 packets), p1 <= p2]

dividers :: [Packet]
dividers = read "[ [[2]], [[6]] ]"

part2 :: [Packet] -> Int
part2 signal = product . map (+ 1) . findIndices (`elem` dividers) . sort $ signal ++ dividers

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . (if null args then part1 else part2) . parseList
