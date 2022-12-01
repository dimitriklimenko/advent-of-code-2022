#!/usr/bin/env -S runhaskell -i..

import AdventIO2 (readListListInts)

day1 :: [[Integer]] -> Integer
day1 = maximum . map sum

main :: IO ()
main = readListListInts >>= print . day1
