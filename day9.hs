#!/usr/bin/env runhaskell

import Data.List (scanl')
import GHC.Utils.Misc (ordNub)
import System.Environment (getArgs)

import Utils (splitPairOn)

data Dir = R | L | U | D
    deriving (Eq, Show)

readDir :: String -> Dir
readDir "R" = R
readDir "L" = L
readDir "U" = U
readDir "D" = D

data Move = Move Dir Int
    deriving (Eq, Show)

readMove :: String -> Move
readMove s = Move (readDir ds) (read ns)
    where (ds, ns) = splitPairOn " " s

data Pos = Pos
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord, Show)

data State = State
    { pHead :: Pos
    , pTail :: Pos
    } deriving (Show)

initState :: State
initState = State (Pos 0 0) (Pos 0 0)

moveHead :: Dir -> Pos -> Pos
moveHead R (Pos hx hy) = Pos (hx + 1) hy
moveHead L (Pos hx hy) = Pos (hx - 1) hy
moveHead U (Pos hx hy) = Pos hx (hy + 1)
moveHead D (Pos hx hy) = Pos hx (hy - 1)

updateTail :: Dir -> Pos -> Pos -> Pos
updateTail R (Pos hx hy) (Pos tx ty)
    | hx - tx >= 2 = Pos (tx + 1) hy
    | otherwise    = Pos tx ty
updateTail L (Pos hx hy) (Pos tx ty)
    | tx - hx >= 2 = Pos (tx - 1) hy
    | otherwise    = Pos tx ty
updateTail U (Pos hx hy) (Pos tx ty)
    | hy - ty >= 2 = Pos hx (ty + 1)
    | otherwise    = Pos tx ty
updateTail D (Pos hx hy) (Pos tx ty)
    | ty - hy >= 2 = Pos hx (ty - 1)
    | otherwise    = Pos tx ty

moveOne :: State -> Dir -> State
moveOne (State hPos tPos) dir = State hPos2 (updateTail dir hPos2 tPos)
    where hPos2 = moveHead dir hPos

listStates :: [Move] -> [State]
listStates = scanl' moveOne initState . concatMap (\(Move dir n) -> replicate n dir)

part1 :: [Move] -> Int
part1 = length . ordNub . map pTail . listStates

main :: IO ()
main = do
    args <- getArgs
    getContents >>= print . part1 . map readMove . lines
