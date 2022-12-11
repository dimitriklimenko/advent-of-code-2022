#!/usr/bin/env runhaskell

import Data.Char (toUpper)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)

data Instruction = Noop | Addx Int
    deriving (Eq, Read, Show)

parseInstruction :: String -> Instruction
parseInstruction (c:rest) = read $ toUpper c:rest

data State = State
    { xReg :: Int
    , cycleNo :: Int
    } deriving (Show)

initState :: State
initState = State 1 1

process :: State -> Instruction -> State
process (State x cycle) Noop         = State x (cycle+1)
process (State x cycle) (Addx delta) = State (x+delta) (cycle+2)

simulate :: State -> [Instruction] -> [State]
simulate = scanl process

getX :: Int -> [State] -> (Int, [State])
getX target history@(s:rest)
    | null rest || (cycleNo . head $ rest) > target = (xReg s, history) 
    | otherwise                                     = getX target rest

part1 :: [Int] -> [State] -> Int
part1 cycles history = fst $ foldl (\(curr, currHist) target ->
    let (x, newHist) = getX target currHist
    in (curr + (x * target), newHist)) (0, history) cycles

part2 :: [Int] -> [State] -> [[Char]]
part2 cycles history = chunksOf 40 . map fst . tail $ scanl (\(_, currHist) target ->
    let (x, newHist) = getX target currHist
    in (if abs (x - (target `mod` 40) + 1) <= 1 then '#' else '.', newHist)) (' ', history) cycles

main :: IO ()
main = do
    args <- getArgs
    if null args
        then getContents >>= print . part1 [20, 60, 100, 140, 180, 220] . simulate initState . map parseInstruction . lines
        else getContents >>= mapM_ putStrLn . part2 [1..240] . simulate initState . map parseInstruction . lines
