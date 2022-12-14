#!/usr/bin/env runhaskell

import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadPrec (ReadPrec, choice)
import Text.Read (Read, readPrec)

import Utils (mapPair, splitPairOn)

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

parseSignal :: String -> [(Packet, Packet)]
parseSignal = map (mapPair read . splitPairOn "\n") . splitOn "\n\n"

sumOrderedIndices :: [(Packet, Packet)] -> Int
sumOrderedIndices signal = sum [i | (i, (p1, p2)) <- zip [1..] signal, p1 <= p2]

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . sumOrderedIndices . parseSignal
