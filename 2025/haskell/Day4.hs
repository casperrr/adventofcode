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

e1 :: Set Pos
e1 = parse e1'

parse :: String -> Set Pos
parse s = S.fromList . map fst . filter ((== '@') . snd) $ [((i,j),x) | (i,row) <- zip [0..] (lines s), (j,x) <- zip [0..] row]

day4Input :: IO (Set Pos)
day4Input = parse <$> fetchBodyStr (inputAOCURL 4)

kernel :: Int -> Pos -> [Pos]
kernel k (x,y) = [(x', y') |
                   x' <- [(x - k) .. (x + k)],
                   y' <- [(y - k) .. (y + k)],
                   x' /= x || y' /= y]

adj :: Pos -> (Pos, Set Pos)
adj p = (p, (S.fromList . kernel 1) p)

adjR :: Set Pos -> Set (Pos, Set Pos)
adjR ps = S.map (\p -> let (_,x) = adj p in (p, S.intersection x ps)) ps

acc :: Set (Pos, Set Pos) -> Set (Pos, Set Pos)
acc = S.filter (\(_,x) -> S.size x < 4)

solve1 :: Set Pos -> Int
solve1 = S.size . acc . adjR

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

solve2 :: Set Pos -> Int
solve2 = sum . removeAll

-----------------------------------------------------------------

day4 :: IO Int
day4 = solve1 <$> day4Input

day4' :: IO Int
day4' = solve2 <$> day4Input