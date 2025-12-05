{-# LANGUAGE TupleSections #-}
module Day4 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.Set (Set, (\\))
import qualified Data.Set as S

type Pos = (Int, Int)

e1' :: String
e1' = "..@@.@@@@.\n\
      \@@@.@.@.@@\n\
      \@@@@@.@.@@\n\
      \@.@@@@..@.\n\
      \@@.@@@@.@@\n\
      \.@@@@@@@.@\n\
      \.@.@.@.@@@\n\
      \@.@@@.@@@@\n\
      \.@@@@@@@@.\n\
      \@.@.@@@.@."

e1 :: [Pos]
e1 = parse e1'

-- parse :: String -> Set Pos
-- parse s = S.fromList . map fst . filter ((== '@') . snd) $ [((i,j),x) | (i,row) <- zip [0..] (lines s), (j,x) <- zip [0..] row]
parse :: String -> [Pos]
parse s = map fst . filter ((== '@') . snd) $ [((i,j),x) | (i,row) <- zip [0..] (lines s), (j,x) <- zip [0..] row]

day4Input :: IO [Pos]
day4Input = parse <$> fetchBodyStr (inputAOCURL 4)

kernel :: Int -> Pos -> [Pos]
kernel k (x,y) = [(x', y') |
                   x' <- [(x - k) .. (x + k)],
                   y' <- [(y - k) .. (y + k)],
                   x' /= x || y' /= y]

adjacent :: Pos -> Set Pos
adjacent = S.fromList . kernel 1

adjRolls :: [Pos] -> [Set Pos]
adjRolls ps = map (S.intersection (S.fromList ps) . adjacent) ps

-- adjacent :: [Pos] -> [Set Pos]
-- adjacent = map (S.fromList <$> kernel 1)

-- filtRolls :: [Pos] -> [Set Pos]
-- filtRolls cs = map (S.intersection $ S.fromList cs) (adjacent cs)

findAcc :: [Set Pos] -> [Set Pos]
findAcc = filter (\x -> S.size x < 4)

count :: [Set Pos] -> Int
count = length . findAcc

solve1 :: [Pos] -> Int
solve1 = count . adjRolls

-----------------------------------------------------------------
-- nah so whos a cunt then
-----------------------------------------------------------------

parse' :: String -> Set Pos
parse' = S.fromList . parse

e2 :: Set Pos
e2 = parse' e1'

adj :: Pos -> (Pos, Set Pos)
adj p = (p, adjacent p)

adjR :: Set Pos -> Set (Pos, Set Pos)
adjR ps = S.map (\p -> let (_,x) = adj p in (p, S.intersection x ps)) ps

acc :: Set (Pos, Set Pos) -> Set (Pos, Set Pos)
acc = S.filter (\(_,x) -> S.size x < 4)

solve1' :: Set Pos -> Int
solve1' = S.size . acc . adjR

-----------------------------------------------------------------
-- Part 2 - Removing rolls
-----------------------------------------------------------------

removed :: Set Pos -> Set Pos
removed = S.map fst . acc . adjR

removeAll :: Set Pos -> [Int]
removeAll ps
    | S.null ps' = []
    | otherwise = length ps' : removeAll (ps \\ ps')
    where ps' = removed ps

solve2' :: Set Pos -> Int
solve2' = sum . removeAll