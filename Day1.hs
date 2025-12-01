module Day1 where
-- Advent of code - Day 1

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