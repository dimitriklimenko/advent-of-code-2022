#!/usr/bin/env runhaskell

import Control.Monad.Loops (whileJust)
import Control.Monad.State.Lazy (State, evalState, runState, state)
import Data.Either.Combinators (fromRight')
import Data.Maybe (Maybe, fromJust)
import System.Environment (getArgs)
import Text.Parsec (Parsec, endBy, many, many1, parse, try, (<|>))
import Text.Parsec.Char (char, digit, noneOf, string)


-- Parsing the input
data ListingEntry = Dir String | File Int String
    deriving Show

data Command = CD String | LS [ListingEntry]
    deriving Show

name :: Parsec String () String
name = many1 (noneOf " \n")

number :: Parsec String () Int
number = read <$> many1 digit

entry :: Parsec String () ListingEntry
entry = try (string "dir " >> Dir <$> name)
    <|> (File <$> number <* char ' ' <*> name)

listing :: Parsec String () [ListingEntry]
listing = endBy entry (char '\n')

cmd :: Parsec String () Command
cmd = do
    string "$ "
    try (string "cd " >> CD <$> name <* char '\n')
        <|> (string "ls\n" >> LS <$> listing)

session :: Parsec String () [Command]
session = many cmd


-- Processing into a filesystem tree
data Node = FileNode Int | DirNode [Node]
    deriving Show

buildDirectory :: [Command] -> (Maybe Node, [Command])
-- End of commands or "cd .." terminates the directory.
buildDirectory []             = (Nothing, [])
buildDirectory (CD "..":rest) = (Nothing, rest)
-- Otherwise we pull files out of the "ls" command, and process all child directories.
buildDirectory (CD path:rest) = runState (do
    files <- state (\(LS listing:rest) -> ([FileNode x | File x _ <- listing], rest))
    dirs <- whileJust (state buildDirectory) return
    return . Just . DirNode $ files ++ dirs) rest


-- Processing the filesystem tree into an answer
totalSize :: Node -> Int
totalSize (FileNode sz)      = sz
totalSize (DirNode children) = sum . map totalSize $ children

processTree :: (a -> a -> a) -> (Int -> a -> a) -> a -> Node -> a
processTree combine agg baseValue = snd . helper
    where helper (FileNode sz)   = (sz, baseValue)
          helper (DirNode nodes) =
            let (sz, combinedValue) = foldr ((\(a, b) (c, d) -> (a+c, combine b d)) . helper) (0, baseValue) nodes
            in (sz, agg sz combinedValue)

data Topped t = Val t | Top deriving (Show, Eq, Ord)

maxSize :: Int
maxSize = 100000
maxSpace :: Int
maxSpace = 70000000
neededSpace :: Int
neededSpace = 30000000

part1 :: Node -> Int
part1 = processTree (+) (\sz tally -> if sz <= maxSize then sz+tally else tally) 0

part2 :: Node -> Int
part2 root = x
    where spaceToFree = neededSpace + totalSize root - maxSpace
          Val x       = processTree min (\sz best -> min best (if sz >= spaceToFree then Val sz else Top)) Top root

main :: IO ()
main = do
    args <- getArgs
    let handler = if null args then part1 else part2
    getContents >>= print . handler . fromJust . evalState (state buildDirectory) . fromRight' . parse session ""
