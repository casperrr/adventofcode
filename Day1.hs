-- Advent of code - Day 1
module Day1 where

import Util.FetchInput
import Network.HTTP.Simple
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor

data Turn = L Int | R Int deriving (Show, Eq)
type Pos = Int

dial :: Pos
dial = 50

turn :: Pos -> Turn -> Pos
turn d (L n) = (d - n) `mod` 100
turn d (R n) = (d + n) `mod` 100

e1 :: [Turn]
e1 = [L 68, L 30, R 48, L 5, R 60, L 55, L 1, L 99, R 14, L 82]

positions :: [Turn] -> [Pos]
positions = scanl turn dial

-- scanl :: forall b a. (b -> a -> b) -> b -> [a] -> [b]
-- scanl :: forall Pos Turn. (Pos -> Turn -> Pos) -> Pos -> [Turn] -> [Pos]

password :: [Turn] -> Int
password ts = length $ filter (== 0) $ positions ts

-- Part 2 - password method 0x434C49434B

passwordMethod2 :: [Turn] -> Int
passwordMethod2 = length . filter (== 0) . allPositions
    where
        allPositions :: [Turn] -> [Pos]
        allPositions turns = concatMap expandTurn (zip (positions turns) turns)
        
        expandTurn :: (Pos, Turn) -> [Pos]
        expandTurn (start, L n) = take n $ iterate (\p -> (p - 1) `mod` 100) start
        expandTurn (start, R n) = take n $ iterate (\p -> (p + 1) `mod` 100) start

-- Above is inefficient but i spent too long trying to do it better

------------------------------------------------------------------------
-- Getting problem input
------------------------------------------------------------------------

day1InputUrl :: String
day1InputUrl = "https://adventofcode.com/2025/day/1/input"

bsToTurns :: ByteString -> [Turn]
bsToTurns = map parse . lines . BS.unpack
    where
        parse ('L':n) = L (read n)
        parse ('R':n) = R (read n)

day1Input :: IO [Turn]
day1Input = fetchAOCBody day1InputUrl <&> bsToTurns

------------------------------------------------------------------------

day1 :: IO Int
day1 = password <$> day1Input

day1' :: IO Int
day1' = passwordMethod2 <$> day1Input