module Day5 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List.Split ( splitOn )
import Data.List ( nub, sortOn )
import Data.Functor ( (<&>) )

type Id = Int
type Range = (Id, Id)
type Input = ([Range], [Id])

e1' :: String
e1' = "3-5\n\
       \10-14\n\
       \16-20\n\
       \12-18\n\
       \\n\
       \1\n\
       \5\n\
       \8\n\
       \11\n\
       \17\n\
       \32"
e1 :: Input
e1 = parse e1'

day5Input :: IO Input
day5Input = parse <$> fetchBodyStr (inputAOCURL 5)

parse :: String -> Input
parse s = (iids, aids)
    where
        [iid, aid] = splitOn "\n\n" s
        pRange r = let [a,b] = splitOn "-" r in (read a, read b)
        iids = lines iid <&> pRange
        aids = lines aid <&> read

fresh :: Input -> [Id]
fresh (rgs, ids) = filter (inRanges rgs) ids

inRange :: Range -> Id -> Bool
inRange (a,b) id = id >= a && id <= b

inRanges :: [Range] -> Id -> Bool
inRanges rgs id = any (`inRange` id) rgs

solve1 :: Input -> Int
solve1 = length . fresh

-------------------------------------------------------------
-- Part 2 - 
-------------------------------------------------------------

-- naive attempt
naive :: [Range] -> [Id]
naive rs = nub $ concatMap (\(a,b) -> [a..b]) rs

rsize :: Range -> Int
rsize (a,b) = b - a + 1

reduce :: [Range] -> [Range]
reduce = foldr merge [] . sortOn fst

merge :: Range -> [Range] -> [Range]
merge r [] = [r]
merge r1@(a1,b1) (r2@(a2,b2):rs)
    | b1 >= a2  = (a1, max b1 b2):rs
    | otherwise = r1:r2:rs

solve2 :: Input -> Int
solve2 = sum . map rsize . reduce . fst

-------------------------------------------------------------

day5 :: IO Int
day5 = solve1 <$> day5Input

day5' :: IO Int
day5' = solve2 <$> day5Input