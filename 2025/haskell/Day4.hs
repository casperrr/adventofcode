{-# LANGUAGE TupleSections #-}
module Day4 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List
import Control.Monad

type Grid = [[Char]]
type Pos = (Int, Int) -- (x, y)

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

e1 :: Grid
e1 = lines e1'

getPos :: Pos -> Grid -> Char
getPos (x,y) g = g !! y !! x

genPos' :: Grid -> [Pos]
genPos' g = [(x,y) | y <- [0..length g-1], x <- [0..length (head g)-1]]

-- genPos :: Grid -> [Pos]
-- genPos g = filter isAt $ genPos g
--     where isAt p = getPos p g == '@'

genPos :: Grid -> [Pos]
genPos = concat . zipWith (\y xs -> map (y,) xs) [0..] . map (elemIndices '@')

genPosSafe :: Grid -> [Pos]
genPosSafe g = filter (\(x,y) -> x /= 0 && y /= 0 && x /= length (head g)-1 && y /= length g-1) $ genPos g

kernel :: Int -> Grid -> Pos -> Grid
kernel s g (x,y) = [[getPos (x', y') g | x' <- [(x-s)..(x+s)]] | y' <- [(y-s)..(x+s)]]

adjacent :: Grid -> Pos -> Grid
adjacent = kernel 1

countAt :: Grid -> Int
countAt = length . concatMap (filter (== '@'))

allAdjacent :: Grid -> [Grid]
allAdjacent = map <$> adjacent <*> genPosSafe

solve1 :: Grid -> Int
solve1 = sum . filter (<4) . map countAt . allAdjacent
