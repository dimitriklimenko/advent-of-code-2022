#!/usr/bin/env runhaskell

import GHC.Utils.Misc (ordNub)
import System.Environment (getArgs)

data Dir = R | L | U | D
    deriving (Eq, Read, Show)

data Move = Move Dir Int
    deriving (Eq, Read, Show)

parseMove :: String -> Move
parseMove s = read $ "Move  " ++ s

data KnotPos = KnotPos
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord, Show)

type State = [KnotPos]

initState :: Int -> State
initState n = replicate n (KnotPos 0 0)

moveHead :: Dir -> KnotPos -> KnotPos
moveHead R (KnotPos tx ty) = KnotPos (tx + 1) ty
moveHead L (KnotPos tx ty) = KnotPos (tx - 1) ty
moveHead U (KnotPos tx ty) = KnotPos tx (ty + 1)
moveHead D (KnotPos tx ty) = KnotPos tx (ty - 1)

follow :: KnotPos -> KnotPos -> KnotPos
follow (KnotPos hx hy) (KnotPos tx ty)
    | abs dx <= 1 && abs dy <= 1 = KnotPos tx ty
    | otherwise                  = KnotPos (tx + signum dx) (ty + signum dy)
    where dx  = hx - tx
          dy  = hy - ty

updateRope :: State -> Dir -> State
updateRope (head:rest) dir = scanl1 follow $ moveHead dir head:rest

simulate :: State -> [Move] -> [State]
simulate state = scanl updateRope state . concatMap (\(Move dir n) -> replicate n dir)

countTailPosns :: Int -> [Move] -> Int
countTailPosns nKnots = length . ordNub . map last . simulate (initState nKnots)

main :: IO ()
main = do
    args <- getArgs
    let nKnots = if null args then 2 else read . head $ args
    getContents >>= print . countTailPosns nKnots . map parseMove . lines
