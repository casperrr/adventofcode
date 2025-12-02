module Day2 where

import Util.FetchInput
import Data.List.Split (splitOn)
import System.IO

type Id = String
type Range = (Id, Id)

e1 :: [Range]
e1 = parse "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

day2Input :: IO String
day2Input = fetchBodyStr (inputAOCURL 2)

day2Input' :: IO [Range]
day2Input' = parse <$> day2Input

parse :: String -> [Range]
parse i = map parseRange $ splitOn "," (filter (/= '\n') i)
    where parseRange r = let [a,b] = splitOn "-" r in (a,b)

makeRange :: Range -> [Int]
makeRange (a,b) = [read a .. read b]

makeRange' :: Range -> [Id]
makeRange' (a,b) = map show [read a .. read b :: Int]

ranges :: [Range] -> [Id]
ranges = concatMap makeRange'

invalid :: Id -> Bool
invalid = or . sequence [leadZero, repSeq]

leadZero :: Id -> Bool
leadZero id = head id == '0'

repSeq :: Id -> Bool
repSeq id = a == b
    where (a,b) = splitAt (length id `div` 2) id

solve1 :: [Range] -> Int
solve1 = sum . map read . filter invalid . ranges

