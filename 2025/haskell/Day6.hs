module Day6 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List
import Data.List.Split

e1 :: String
e1 = "123 328  51 64 \n\
     \ 45 64  387 23 \n\
     \  6 98  215 314\n\
     \*   +   *   +  "

input :: IO String
input = fetchBodyStr (inputAOCURL 6)

chunks :: String -> [Int]
chunks = tail . map ((+1) . length) . splitOneOf "*+" . last . lines

parse :: String -> [[String]]
parse = transpose . (map . splitPlaces . chunks <*> lines)

eval :: [String] -> Int
eval es = foldr1 op $ read <$> init es
    where op
            | head (last es) == '*' = (*)
            | head (last es) == '+' = (+)

solve1 :: String -> Int
solve1 = sum . map eval . parse

--------------------------------------------------------------
-- Part 2 - new maths
--------------------------------------------------------------

fix :: [String] -> [String]
fix es = init (head es) : tail clean ++ [[last (head es)]]
    where clean = filter (not . all (== ' ')) es

part2 :: String -> [[String]]
part2 = map (fix . transpose) . parse

solve2 :: String -> Int
solve2 = sum . map eval . part2