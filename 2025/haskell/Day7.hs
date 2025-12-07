module Day7 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List

type Input = [String]
type Splits = [Int]
type Beams = [Int]

e1' :: String
e1' = ".......S.......\n\
      \...............\n\
      \.......^.......\n\
      \...............\n\
      \......^.^......\n\
      \...............\n\
      \.....^.^.^.....\n\
      \...............\n\
      \....^.^...^....\n\
      \...............\n\
      \...^.^...^.^...\n\
      \...............\n\
      \..^...^.....^..\n\
      \...............\n\
      \.^.^.^.^.^...^.\n\
      \..............."

e1 :: Input
e1 = lines e1'
    
input :: IO Input
input = lines <$> fetchBodyStr (inputAOCURL 7)

sBeam :: Input -> Beams
sBeam = elemIndices 'S' . head

splitters :: Input -> [Splits]
splitters = filter (not . null) . map (elemIndices '^') . tail

split :: Splits -> Beams -> Beams
split ss bs = sort . nub $ new `union` non
    where
        col = ss `intersect` bs
        new = concatMap (\x -> [x-1,x+1]) col
        non = bs \\ ss

-- not currently used but gets all beams from a list of splitters
beams :: Beams -> [Splits] -> [[Int]]
beams _  []     = []
beams bs (x:xs) = bs : beams (split x bs) xs

prick' :: [Splits] -> Beams -> [Int]
prick' []      _  = []
prick' (x :xs) ys = length (ys `intersect` x) : prick' xs (split x ys)

prick :: Input -> [Int]
prick bs = prick' (splitters bs) (sBeam bs)

solve1 :: Input -> Int
solve1 = sum . prick