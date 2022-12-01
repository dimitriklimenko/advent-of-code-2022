module AdventIO2 (parseLinesIntoListListInts, readListListInts) where

processToken :: String -> [[Integer]] -> [[Integer]]
processToken ""    lists = [] : lists
processToken token lists = case lists of
    []             -> [[n]]
    current : rest -> (n:current):rest
    where n = read token

parseLinesIntoListListInts :: [String] -> [[Integer]]
parseLinesIntoListListInts tokens = foldr processToken [] tokens


readListListInts :: IO [[Integer]]
readListListInts = getContents >>= return . parseLinesIntoListListInts . lines 
