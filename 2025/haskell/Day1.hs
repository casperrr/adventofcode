-- Advent of code - Day 1
module Day1 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ( (<&>) )

data OldTurn = L Int | R Int deriving (Show, Eq)
type Turn = Int
type Pos = Int

dial :: Pos
dial = 50

-- turn :: Pos -> Turn -> Pos
-- turn d (L n) = (d - n) `mod` 100
-- turn d (R n) = (d + n) `mod` 100

turn :: Pos -> Turn -> Pos
turn p t = (p+t) `mod` 100

e1 :: [Turn]
e1 = map oldTurnToNew [L 68, L 30, R 48, L 5, R 60, L 55, L 1, L 99, R 14, L 82]

oldTurnToNew :: OldTurn -> Turn
oldTurnToNew (L n) = negate n
oldTurnToNew (R n) = n

positions :: [Turn] -> [Pos]
positions = scanl turn dial

-- scanl :: forall b a. (b -> a -> b) -> b -> [a] -> [b]
-- scanl :: forall Pos Turn. (Pos -> Turn -> Pos) -> Pos -> [Turn] -> [Pos]

password :: [Turn] -> Int
password ts = length $ filter (== 0) $ positions ts

-- Part 2 - password method 0x434C49434B

passwordMethod2' :: [Turn] -> Int
passwordMethod2' = length . filter (== 0) . allPositions
    where
        allPositions :: [Turn] -> [Pos]
        allPositions turns = concatMap expandTurn (zip (positions turns) turns)
        
        expandTurn :: (Pos, Turn) -> [Pos]
        expandTurn (start, n) = take (abs n) $ iterate (\p -> (p + signum n) `mod` 100) start

-- Above is inefficient but i spent too long trying to do it better

passwordMethod2 :: [Turn] -> Int
passwordMethod2 ts = sum $ zipWith zeros (positions ts) ts
    where zeros p t
            | t > 0     = (p+t) `div` 100
            | otherwise = (p+t) `div` (-100) - p `div` (-100)

------------------------------------------------------------------------
-- Getting problem input
------------------------------------------------------------------------

strToTurns :: String -> [Turn]
strToTurns = map parse . lines
    where
        parse ('L':n) = negate $ read n
        parse ('R':n) = read n

day1Input :: IO [Turn]
day1Input = fetchBodyStr (inputAOCURL 1) <&> strToTurns

------------------------------------------------------------------------

day1 :: IO Int
day1 = password <$> day1Input

day1' :: IO Int
day1' = passwordMethod2 <$> day1Input