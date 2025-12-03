module Day3 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )

type Bat = Int
type Bank = [Bat]
type Jolts = Int

e1 :: [Bank]
e1 = parse "987654321111111\n811111111111119\n234234234234278\n818181911112111"

day3Input :: IO [Bank]
day3Input = parse <$> fetchBodyStr (inputAOCURL 3)

parse :: String -> [Bank]
parse = (map . map) (read . pure) . lines

largest :: Bank -> Jolts
largest = largest' 2

solve1 :: [Bank] -> Jolts
solve1 = sum . map largest

--------------------------------------------------------------------------------------
-- Part 2 - 12 batteries on
--------------------------------------------------------------------------------------

takeSec :: Int -> [a] -> [a]
takeSec n as = take (length as - n + 1) as

output :: [Bat] -> Jolts
output = read . concatMap show

largest' :: Int -> Bank -> Jolts
largest' n = output . go n
  where
    go :: Int -> [Bat] -> [Bat]
    go 0 _  = []
    go k bs = max : go (k-1) rest
      where
        max  = (maximum . takeSec k) bs
        rest = tail $ dropWhile (/= max) bs

solve2 :: [Bank] -> Jolts
solve2 = sum . map (largest' 12)

-----------------------------------------------------

day3 :: IO Int
day3 = solve1 <$> day3Input

day3' :: IO Int
day3' = solve2 <$> day3Input