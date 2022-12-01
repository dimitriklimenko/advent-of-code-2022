{-# LANGUAGE LambdaCase #-}

module AdventIO (readInt, readListInts, readListListInts) where

-- import Control.Monad
import Data.Bifunctor (bimap)
import System.IO (isEOF)
import Text.Read (readMaybe)

readInt :: IO (Maybe Integer)
readInt = getLine >>= return . readMaybe

readListInts :: IO (Bool, [Integer])
readListInts = isEOF >>= \case
    True -> return (True, [])
    False -> readInt >>= \case
        Nothing -> return (False, [])
        Just x -> do
            (done, xs) <- readListInts
            return (done, x : xs)

readListListInts :: IO [[Integer]]
readListListInts = readListInts >>= \case
    (True, []) -> return []
    (True, xs) -> return [xs]
    (False, xs) -> readListListInts >>= return . (xs :)

