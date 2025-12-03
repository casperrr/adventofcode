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
largest bs = read $ show l1 ++ show l2
    where
        l1 = maximum $ init bs
        l2 = (maximum . tail) $ dropWhile (/= l1) bs

solve1 :: [Bank] -> Int
solve1 = sum . map largest

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------


-- (.)         :: (b1 -> c) ->  -> (a1 -> c)
-- map         :: (a2 -> b2) -> [a2] -> [b2]
-- map         :: (a3 -> b3) -> [a3] -> [b3]
-- (.)         :: ((a2 -> b2) -> ([a2] -> [b2])) -> (a1 -> (a2 -> b2)) -> (a1 -> ([a2] -> [b2]))
-- (.) map     :: (a1 -> (a2 -> b2)) -> (a1 -> ([a2] -> [b2]))
-- (.) map     :: ((a3 -> b3) -> ([a3] -> [b3])) -> ((a3 -> b3) -> ([[a3]] -> [[b3]]))
-- (.) map map :: ((a3 -> b3) -> ([[a3]] -> [[b3]]))
-- (.) map map :: (a3 -> b3) -> [[a3]] -> [[b3]]

-- (.)      :: (b1 -> c) -> (a1 -> b1) -> a1 -> c
-- pure     :: Applicative f => a2 -> f a2
-- read     :: Read a3 => String -> a3
-- (.)      :: Read a3 => (String -> a3) -> a1 -> String -> (a1 -> a3)
-- (.) read :: Read a3 => (a1 -> String) -> a1 -> a3
-- (.) read :: Read a3 => (a1 -> [Char]) -> a1 -> a3
-- pure     :: a2 -> [a2]
-- (.) read pure :: Read a3 => Char -> a3

-- (map . map)   :: (a1 -> b1) -> [[a1]] -> [[b1]]
-- (read . pure) :: Read a2 => Char -> a3
-- (map . map) (read . pure) :: Read a2 => [[Char]] -> [[a2]]
-- (map . map) (read . pure) :: Read a2 => [String] -> [[a2]]

-- (.)            :: (b1 -> c) -> (a1 -> b1) -> (a1 -> c)
-- lines          :: String -> [String]
-- mmrp           :: Read a2 => [String] -> [[a2]]
-- (.) mmrp       :: Read a2 => (a1 -> [String]) -> (a1 -> [[a2]])
-- (.) mmrp lines :: Read a2 => String -> [[a2]]

-- (mmrp . lines) :: Read a2 => String -> [[a2]]
-- (mmrp . lines) :: String -> [[Int]]