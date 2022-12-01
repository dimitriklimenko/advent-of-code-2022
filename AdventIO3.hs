module AdventIO3 (parseListListInts, readListListInts) where

import Data.List.Split

parseListListInts :: String -> [[Integer]]
parseListListInts = map (map read . filter (not . null) . splitOn "\n") . splitOn "\n\n"

readListListInts :: IO [[Integer]]
readListListInts = getContents >>= return . parseListListInts
