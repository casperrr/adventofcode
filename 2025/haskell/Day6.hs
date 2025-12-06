module Day6 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List
import Data.Functor
import Control.Monad

e1' :: String
e1' = "123 328  51 64 \n\
       \45 64  387 23 \n\
        \6 98  215 314\n\
      \*   +   *   +  "

e1 :: [[String]]
e1 = parse e1'

input :: IO [[String]]
input = parse <$> fetchBodyStr (inputAOCURL 6)


parse :: String -> [[String]]
parse = transpose . map words . lines

eval :: [String] -> Int
eval es = foldr1 op $ read <$> init es
    where op
            | last es == "*" = (*)
            | last es == "+" = (+)

eval' :: [String] -> Int
eval' es = foldr1 (if last es == "*" then (*) else (+)) (read <$> init es)

solve1 :: [[String]] -> Int
solve1 = sum . map eval