module Day5 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List.Split ( splitOn )

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
e1 :: ([Int], [Int])
e1 = parse e1'
e1f :: [Int]
e1f = fst e1
e1a :: [Int]
e1a = snd e1

parse :: String -> ([Int], [Int])
parse s = (fids, aids)
    where
        [iid, aid] = splitOn "\n\n" s
        aids = map read $ lines aid
        fids = lines iid >>= pRange
        pRange r = let [a,b] = splitOn "-" r in [read a .. read b]

