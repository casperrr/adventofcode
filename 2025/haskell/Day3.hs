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

--------------------------------------------------------------------------------------
-- I stole this from [here](https://github.com/RossPaterson/advent-of-code/blob/main/2025/Day03.hs) just so i could figure out how it works NOT MINE! but genius
--------------------------------------------------------------------------------------

solve2' :: [[Int]] -> Int
solve2' = sum . map (maxSubNum 12)

maxSubNum :: Int -> [Int] -> Int
maxSubNum n bs = maxSubNums bs !! (n-1)

maxSubNums :: [Int] -> [Int]
maxSubNums = foldr addDigit []

addDigit :: Int -> [Int] -> [Int]
addDigit d best = zipWith max (best++[0]) (zipWith (+) (iterate (*10) d) (0:best))

-- maxSubNums [4,9,8,1]
-- foldr addDigit [] [4,9,8,1]
-- addDigit 4 $ addDigit 9 $ addDigit 8 $ addDigit 1 []
-- addDigit 4 $ addDigit 9 $ addDigit 8 $ zipWith max ([]++[0]) (zipWith (+) (iterate (*10) 1) (0:[]))
-- zipWith max ([0]) (zipWith (+) (iterate (*10) 1) ([0]))
-- zipWith max ([0]) (zipWith (+) [1,10,100,...] [0])
-- zipWith max [0] [1]
-- [1]
-- addDigit 4 $ addDigit 9 $ addDigit 8 [1]
-- addDigit 4 $ addDigit 9 $ zipWith max ([1]++[0]) (zipWith (+) (iterate (*10) 8) (0:[1]))
-- zipWith max ([1]++[0]) (zipWith (+) (iterate (*10) 8) (0:[1]))
-- zipWith max [1,0] (zipWith (+) [8,80,800,...] [0,1])
-- zipWith max [1,0] [8,81]
-- [8,81]
-- addDigit 4 $ addDigit 9 [8,81]
-- addDigit 4 $ zipWith max ([8,81]++[0]) (zipWith (+) (iterate (*10) 9) (0:[8,81]))
-- zipWith max [8,81,0] (zipWith (+) [9,90,900,9000,...] [0,8,81])
-- zipWith max [8,81,0] [9,98,981]
-- [9,98,981]
-- addDigit 4 [9,98,981]
-- zipWith max ([9,98,981]++[0]) (zipWith (+) (iterate (*10) 4) (0:[9,98,981]))
-- zipWith max ([9,98,981,0]) (zipWith (+) [4,40,400,4000,...] [0,9,98,981])
-- zipWith max [9,98,981,0] [4,49,498,4981]
-- [9,98,981,4981]

-- damn this shit so clever