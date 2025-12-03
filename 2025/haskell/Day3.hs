module Day3 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List

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
largest bs = read $ show l1 ++ show l2
    where
        l1 = maximum $ init bs
        l2 = (maximum . tail) $ dropWhile (/= l1) bs

solve1 :: [Bank] -> Jolts
solve1 = sum . map largest

--------------------------------------------------------------------------------------
-- Part 2 - 12 batteries on
--------------------------------------------------------------------------------------

takeSec :: Int -> [a] -> [a]
takeSec n as = take (length as - n + 1) as

output :: [Bat] -> Jolts
output = read . concatMap show

-----------------------------------------------------------------------------
-- Idk which one of these is cleanest so im in hell choosing so have them all
-----------------------------------------------------------------------------
large1 :: Int -> Bank -> Jolts
large1 n = output . go n
    where
        go :: Int -> [Bat] -> [Bat]
        go 0 _  = []
        go k bs = max : go (k-1) rest
            where
                max  = (maximum . takeSec k) bs
                rest = tail $ dropWhile (/= max) bs

large2 :: Int -> Bank -> Jolts
large2 n = output . go n
    where
        go :: Int -> [Bat] -> [Bat]
        go 0 _  = []
        go k bs = max : go (n-1) (tail $ dropWhile (/= max) bs)
            where max = (maximum . takeSec n) bs

large3 :: Int -> Bank -> Jolts
large3 n = output . take n . go n
    where go n bs = max : go (n-1) (tail $ dropWhile (/= max) bs)
            where max = (maximum . takeSec n) bs
-----------------------------------------------------------------------------

largest' :: Int -> Bank -> Jolts
largest' = large3

solve2 :: [Bank] -> Jolts
solve2 = sum . map (largest' 12)