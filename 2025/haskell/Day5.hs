module Day5 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List.Split ( splitOn )
import Data.List
import Data.Functor

type Id = Int
type Range = (Id, Id)

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
e1 :: ([Range], [Id])
e1 = parse e1'

day5Input :: IO ([Range], [Id])
day5Input = parse <$> fetchBodyStr (inputAOCURL 5)

parse :: String -> ([Range], [Id])
parse s = (iids, aids)
    where
        [iid, aid] = splitOn "\n\n" s
        pRange r = let [a,b] = splitOn "-" r in (read a, read b)
        iids = lines iid <&> pRange
        aids = lines aid <&> read

fresh :: ([Range], [Id]) -> [Id]
fresh (rgs, ids) = filter (inRange rgs) ids

inRange :: [Range] -> Id -> Bool
inRange rgs id = any (\(a,b) -> id >= a && id <= b) rgs

solve1 :: ([Range], [Id]) -> Int
solve1 = length . fresh