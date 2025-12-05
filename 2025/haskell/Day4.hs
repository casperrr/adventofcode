{-# LANGUAGE TupleSections #-}
module Day4 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.Set (Set)
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

adjacent :: Pos -> Set Pos
adjacent = S.fromList <$> kernel 1

fuck :: Set Pos -> [Set Pos]
fuck = map adjacent . S.toList

filtRolls :: Set Pos -> [Set Pos]
filtRolls cs = map (S.intersection cs) (fuck cs)

count :: [Set Pos] -> Int
count = length . filter (<4) . map S.size

solve1 :: Set Pos -> Int
solve1 = count . filtRolls