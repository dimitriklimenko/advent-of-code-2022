#!/usr/bin/env runhaskell

import Data.Either.Combinators (fromRight')
import Data.Foldable (toList)
import Data.List (intercalate, sortOn)
import Data.Ord (Down(..))
import Data.Sequence (Seq, adjust', empty, fromList, index, zipWith, (|>))
import GHC.Utils.Misc (ordNub)
import System.Environment (getArgs)
import Text.Parsec (Parsec, endBy, many, many1, parse, sepBy, try, (<|>))
import Text.Parsec.Char (char, digit, newline, noneOf, spaces, string)

-- Data types and parsing
number :: Parsec String () Int
number = read <$> many1 digit

newtype State = State
    { monkeys :: Seq Monkey
    } deriving (Show)

stateParser :: Parsec String () State
stateParser = State . fromList <$> sepBy monkeyParser newline

data Monkey = Monkey
    { items :: Seq Int
    , operation :: Operation
    , test :: Test
    } deriving (Show)

monkeyParser :: Parsec String () Monkey
monkeyParser = do
    string "Monkey " >> number >> string ":\n  Starting items: "
    items <- itemsParser
    oper <- newline >> operationParser
    test <- newline >> testParser
    newline
    return $ Monkey items oper test

itemsParser :: Parsec String () (Seq Int)
itemsParser = fromList <$> sepBy number (string ", ")

data Operation = Plus Operand Operand | Times Operand Operand
    deriving (Show)

operationParser :: Parsec String () Operation
operationParser = do
    op1 <- string "  Operation: new = " >> operandParser
    operator <- spaces >> (try (char '+' >> return Plus) <|> (char '*' >> return Times))
    op2 <- spaces >> operandParser
    return $ operator op1 op2

data Operand = Old | Num Int
    deriving (Show)

operandParser :: Parsec String () Operand
operandParser = try (string "old" >> return Old) <|> Num <$> number

data Test = Test
    { modulus :: Int
    , trueTarget :: Int
    , falseTarget :: Int
    } deriving (Show)

testParser :: Parsec String () Test
testParser = do
    modulus <- string "  Test: divisible by " >> number
    ifTrue <- string "\n    If true: throw to monkey " >> number
    ifFalse <- string "\n    If false: throw to monkey " >> number
    return $ Test modulus ifTrue ifFalse

-- Processing worry levels
getValue :: Int -> Operand -> Int
getValue old Old     = old
getValue _   (Num x) = x

increaseWorry :: Operation -> Int -> Int
increaseWorry (Plus a b)  old = getValue old a + getValue old b
increaseWorry (Times a b) old = getValue old a * getValue old b

-- Applying the test to see where a monkey throws to
getTarget :: Test -> Int -> Int
getTarget (Test modulus trueTarget falseTarget) worry =
    if worry `mod` modulus == 0
    then trueTarget
    else falseTarget  

-- Processing state updates
inspectAndThrow :: (Int -> Int) -> Monkey -> State -> Int -> State
inspectAndThrow relief (Monkey _ operation test) (State ms) itemWorry =
    State $ adjust' (\(Monkey items o t) -> Monkey (items |> newWorry) o t) target ms
  where
    newWorry = relief . increaseWorry operation $ itemWorry
    target   = getTarget test newWorry

clearItems :: Int -> State -> State
clearItems i (State ms) = State $ adjust' (\(Monkey _ o t) -> Monkey empty o t) i ms

doTurn :: (Int -> Int) -> Int -> State -> (State, Int)
doTurn relief i state = (clearItems i $ foldl (inspectAndThrow relief monkey) state itms, length itms)
  where
    monkey = monkeys state `index` i
    itms   = items monkey

doRound :: (Int -> Int) -> State -> (State, Seq Int)
doRound relief state@(State monkeys) = foldl (\(s, counts) i ->
    let (s2, c) = doTurn relief i s
    in (s2, counts |> c)) (state, empty) [0..length monkeys - 1]

initCounts :: State -> Seq Int
initCounts (State monkeys) = fromList $ replicate (length monkeys) 0

simulate :: (Int -> Int) -> Int -> State -> (State, Seq Int)
simulate relief numRounds initState = foldl (\(state, counts) _ ->
    let (newState, extraCounts) = doRound relief state
    in (newState, Data.Sequence.zipWith (+) counts extraCounts)) (initState, initCounts initState) [1..numRounds]

part1 :: State -> Int
part1 = product . take 2 . sortOn Down . toList . snd . simulate (`div` 3) 20

part2 :: State -> Int
part2 state = product . take 2 . sortOn Down . toList . snd . simulate (`mod` sharedModulus) 10000 $ state
  where sharedModulus = foldr (lcm . modulus . test) 1 . toList . monkeys $ state

main :: IO ()
main = do
    args <- getArgs
    let handler = if null args then part1 else part2
    getContents >>= print . handler . fromRight' . parse stateParser ""
