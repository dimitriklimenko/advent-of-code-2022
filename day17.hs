#!/usr/bin/env runhaskell

import qualified Data.Map as Map
import System.Environment (getArgs)

-- Basic data types
data Push = LeftPush | RightPush
    deriving (Show)

data Cell = Empty | Moving | Full
    deriving (Eq, Show)

type Row = [Cell]

-- Parsing and displaying data
charToPush :: Char -> Push
charToPush '<' = LeftPush
charToPush '>' = RightPush

readJetPattern :: String -> [Push]
readJetPattern = map charToPush

toChar :: Cell -> Char
toChar Empty  =  '.'
toChar Moving = '@'
toChar Full   = '#'

fromChar :: Char -> Cell
fromChar '.' = Empty
fromChar '@' = Moving
fromChar '#' = Full

showRow :: Row -> String
showRow = map toChar

showRows :: [Row] -> String
showRows = unlines . map showRow

readRow :: String -> Row
readRow = map fromChar

readRows :: String -> [Row]
readRows = map readRow . lines

-- Default constructs and shapes
full :: Row
full = readRow "#########"

empty :: Row
empty = readRow "#.......#"

horizontal :: [Row]
horizontal = readRows $ unlines [
    "#..@@@@.#"]

cross :: [Row]
cross = readRows $ unlines [
    "#...@...#",
    "#..@@@..#",
    "#...@...#"]

rightAngle :: [Row]
rightAngle = readRows $ unlines [
    "#....@..#",
    "#....@..#",
    "#..@@@..#"]

vertical :: [Row]
vertical = readRows $ unlines [
    "#..@....#",
    "#..@....#",
    "#..@....#",
    "#..@....#"]

square :: [Row]
square = readRows $ unlines [
    "#..@@...#",
    "#..@@...#"]

shapeOrder :: [[Row]]
shapeOrder = [horizontal, cross, rightAngle, vertical, square]

initGrid :: [Row]
initGrid = [full]

-- Shape movement
rowHasOverlap :: Row -> Row -> Bool
rowHasOverlap shapeRow gridRow = (Moving, Full) `elem` zip shapeRow gridRow

hasOverlap :: [Row] -> [Row] -> Bool
hasOverlap shape grid = any (uncurry rowHasOverlap) (zip shape grid)

mergeCell :: Cell -> Cell -> Cell
mergeCell Empty  Empty = Empty
mergeCell _      Full  = Full
mergeCell Moving _     = Full
mergeCell Full   Empty = Empty
mergeCell c1     c2    = error $ "Unexpected merge: " ++ show (c1, c2)

mergeShape :: [Row] -> [Row] -> [Row]
mergeShape shape grid = zipWith (zipWith mergeCell) shape grid ++ drop (length shape) grid

shiftRow :: Push -> Row -> Row
shiftRow LeftPush  row = tail row ++ [Full]
shiftRow RightPush row = Full : init row

pushShape :: Push -> [Row] -> [Row] -> [Row]
pushShape push grid shape = if hasOverlap shiftedShape grid then shape else shiftedShape
  where
    shiftedShape = map (shiftRow push) shape

dropShape :: [Push] -> [Row] -> ([Row], ([Push], Int)) -> ([Row], ([Push], Int))
dropShape jetPattern shape (grid, ([], ji)) = dropShape jetPattern shape (grid, (jetPattern, ji))
dropShape jetPattern shape (grid@(tr:grid'), (p:ps', pi))
    | length shape > length grid = error "Grid too small!?"
    | canDrop                    = (newGrid, (ps'', pi''))
    | otherwise                  = (mergeShape pushedShape grid, (ps', pi'))
  where
    pushedShape            = pushShape p grid shape
    canDrop                = not $ hasOverlap pushedShape grid'
    pi'                    = (pi + 1) `mod` length jetPattern
    (grid'', (ps'', pi'')) = dropShape jetPattern pushedShape (grid', (ps', pi'))
    newGrid                = if tr == empty then grid'' else tr:grid''

addShape :: [Push] -> [Row] -> ([Row], ([Push], Int)) -> ([Row], ([Push], Int))
addShape jetPattern shape (grid, js) = dropShape jetPattern shape (newGrid, js)
  where
    newGrid = replicate (length shape + 3) empty ++ grid

type State = ([Row], ([Push], Int), ([[Row]], Int))

simulate :: [Push] -> State -> State
simulate jetPattern (grid, js, ([], si)) = simulate jetPattern (grid, js, (shapeOrder, si))
simulate jetPattern (grid, js, (s:ss, si)) = (grid', js', (ss, (si + 1) `mod` length shapeOrder))
  where
    (grid', js') = addShape jetPattern s (grid, js)

type HistoryMap = Map.Map (Int, Int) [(Int, Int)]

getTowerHeight :: [Push] -> Int -> Int
getTowerHeight jets r = helper jets r (initState, Map.empty)
  where
    initState = (initGrid, (jets, 0), (shapeOrder, 0))

    helper :: [Push] -> Int -> (State, HistoryMap) -> Int
    helper jets 0 ((grid, _, _), _) = length grid - 1
    helper jets r (state@(grid, (_, ji), (_, si)), history) = case Map.lookup (ji, si) history of
        Nothing  -> baseResult
        Just []  -> baseResult
        Just [_] -> baseResult
        Just ((r', h'):(r'', h''):_)
            | r-r' /= r'-r'' || h-h' /= h' - h'' -> baseResult
            | otherwise -> 
                let newR = (r - 1) `mod` (r' - r)
                    extraHeight = (h - h') * ((r - 1) `div` (r' - r))
                in  extraHeight + helper jets newR (newState, newHistory)
      where
        newState = simulate jets state
        baseResult = helper jets (r - 1) (newState, newHistory)
        h = length grid - 1
        updater :: (Int, Int) -> Maybe [(Int, Int)] -> Maybe [(Int, Int)]
        updater e Nothing   = Just [e]
        updater e (Just es) = Just (e:es)
        newHistory = Map.alter (updater (r, h)) (ji, si) history

main :: IO ()
main = do
    args <- getArgs
    jets <- readJetPattern <$> getLine
    let numCycles = if null args then 2022 else 1000000000000
    print $ getTowerHeight jets numCycles
