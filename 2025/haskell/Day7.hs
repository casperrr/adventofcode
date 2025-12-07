module Day7 where

import Util.FetchInput ( fetchBodyStr, inputAOCURL )
import Data.List

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

e1 :: [String]
e1 = lines e1'

input :: IO [String]
input = lines <$> fetchBodyStr (inputAOCURL 7)

split :: [Int] -> [Int]
split = sort . nub . ([subtract 1, (+1)] <*>)
-- split = nub . concatMap (\x -> [x-1, x+1])

-- splitters :: [String] -> [[Int]]
-- splitters = map (split . elemIndices '^') . tail

-- count' :: [[Int]] -> [Int] -> Int
-- count' [] _ = 0
-- count' ([]:xs) ys = length ys + count' xs ys
-- count' (x :xs) ys
--     | x == split ys = count' xs x
--     | otherwise     = 0

-- count :: [String] -> Int
-- count s = count' (splitters s) (elemIndices 'S' (head s))

idk = elemIndices 'S' . head

idkk = filter (not . null) . map (elemIndices '^') . tail

-- count2' []      _  = []
-- count2' ([]:xs) ys = count2' xs ys
-- count2' (x :xs) ys
--     | x == split ys = length ys : count2' xs x
--     | otherwise     = length x  : count2' xs x

-- cunt []      _  = []
-- cunt ([]:xs) ys = cunt xs ys
-- cunt (x :xs) ys
--     | x == ys       = length ys : cunt xs (split ys)
--     | otherwise     = length (x `intersect` ys) : cunt xs (split (x `intersect` ys))

-- runCunt :: [String] -> [Int]
-- runCunt s = cunt (idkk s) (idk s)

-- cunt [[],[7],[],[6,8],[],[5,7,9],[],[4,6,10],[],[3,5,9,11],[],[2,6,12],[],[1,3,5,7,9,13],[]] [7]
-- length [7] : cunt [[6,8],[],[5,7,9],[],[4,6,10],[],[3,5,9,11],[],[2,6,12],[],[1,3,5,7,9,13],[]] [7]
-- length [7] : length [6,8] : cunt [[5,7,9],[],[4,6,10],[],[3,5,9,11],[],[2,6,12],[],[1,3,5,7,9,13],[]] [5,7,9]
-- length [7] : length [6,8] : length [5,7,9] : cunt [[4,6,10],[],[3,5,9,11],[],[2,6,12],[],[1,3,5,7,9,13],[]] [4,6,8,10]
-- length [7] : length [6,8] : length [5,7,9] : cunt [[4,6,10],[],[3,5,9,11],[],[2,6,12],[],[1,3,5,7,9,13],[]] [4,6,8,10]
-- x = [4,6,8], ys = [4,6,8,10] 

noo :: [Int] -> [Int] -> [Int]
noo = intersect

oon :: [Int] -> [Int] -> [Int]
oon = (\\)

-- rewrite this better please without using 'split'
split2 :: [Int] -> [Int] -> [Int]
-- split2 ss bs = sort . nub $ split (ss `intersect` bs) `union` (ss \\ bs)
split2 ss bs = sort . nub $ new `union` non
    where
        col = ss `intersect` bs
        new = concatMap (\x -> [x-1,x+1]) col
        non = bs \\ ss



prick' :: [[Int]] -> [Int] -> [Int]
prick' []      _  = []
prick' (x :xs) ys = length (ys `intersect` x) : prick' xs (split2 x ys)

prick :: [String] -> [Int]
prick bs = prick' (idkk bs) (idk bs)

solve1 :: [String] -> Int
solve1 = sum . prick

splitters :: [String] -> [[Int]]
splitters = idkk

beams :: [Int] -> [[Int]] -> [[Int]]
beams _  []     = []
beams bs (x:xs) = bs : beams (split2 x bs) xs
-- beams bs (x:xs) = let bs' = split2 x bs in bs' : beams bs' xs

poo = map (elemIndices '|') . lines <$> readFile "temp.txt"

hmm [] = []
hmm (_:x:xs) = x : hmm xs

peen = do
    l1 <- hmm <$> poo
    l2 <- (beams [70] <$> splitters) . take 28 <$> input
    print l1
    print l2
    return $ zipWith (==) l1 l2




-- count2' [[],[6,8],[],[5,7,9],[],[4,6,8,10],[],[3,5,7,9,11],[],[2,4,6,8,10,12],[],[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [7]
-- count2' [[6,8],[],[5,7,9],[],[4,6,8,10],[],[3,5,7,9,11],[],[2,4,6,8,10,12],[],[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [7]
-- length [7] : count2' [[],[5,7,9],[],[4,6,8,10],[],[3,5,7,9,11],[],[2,4,6,8,10,12],[],[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [6,8]
-- length [7] : length [6,8] : count2' [[],[4,6,8,10],[],[3,5,7,9,11],[],[2,4,6,8,10,12],[],[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [5,7,9]
-- length [7] : length [6,8] : length [5,7,9] : count2' [[3,5,7,9,11],[],[2,4,6,8,10,12],[],[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [4,6,8,10]
-- length [7] : length [6,8] : length [5,7,9] : length [4,6,8,10] : count2' [[2,4,6,8,10,12],[],[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [3,5,7,9,11]
-- length [7] : length [6,8] : length [5,7,9] : length [4,6,8,10] : length [3,5,7,9,11] : count2' [[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [2,4,6,8,10,12]
-- length [7] : length [6,8] : length [5,7,9] : length [4,6,8,10] : length [3,5,7,9,11] : count2' [[1,3,5,7,11,13],[],[0,2,4,6,8,10,12,14],[]] [2,4,6,8,10,12]
-- split [2,4,6,8,10,12] = [1,3,5,7,9,11,13]
-- [1,3,5,7,9,11,13] /= [1,3,5,7,11,13]

no :: [Int] -> [Int] -> [Int]
no = (\\)

-- (<$>)     :: Functor f => (a -> b) -> f a -> f b
-- (:)       :: a -> [a] -> [a]
-- (<$>) (:) :: Functor f => (a -> b) -> f a -> f b
-- (<$>) (:) :: Functor f => (a -> ([a] -> [a])) -> f a -> f ([a] -> [a])
-- (<$>) (:) :: Functor f => f a -> f ([a] -> [a])

-- (<$>) (:)      :: Functor f => f a -> f ([a] -> [a])
-- (p1)           :: [String] -> [Int]
-- (<$>) (:) (p1) :: Functor f => f a -> f ([a] -> [a])
-- (<$>) (:) (p1) :: Functor (r ->) => (r -> a) -> (r -> ([a] -> [a]))
-- (<$>) (:) (p1) :: (r -> a) -> (r -> ([a] -> [a]))
-- (<$>) (:) (p1) :: ([String] -> [Int]) -> ([String] -> ([[Int]] -> [[Int]]))
-- (<$>) (:) (p1) :: [String] -> [[Int]] -> [[Int]]

-- (<*>)                  :: Applicative f => f (a -> b) -> f a -> f b
-- (<$>) (:) (p1)         :: [String] -> [[Int]] -> [[Int]]
-- f (a -> b) with ([String] -> [[Int]] -> [[Int]])
-- f = ((->) [String])
-- 
-- (<*>) ((<$>) (:) (p1)) :: Applicative ([String] ->) => ([String] -> (a -> b)) -> ([String] -> a) -> ([String] -> b)
-- (<*>) ((<$>) (:) (p1)) :: ([String] -> (a -> b)) -> ([String] -> a) -> ([String] -> b)
-- (<*>) ((<$>) (:) (p1)) :: ([String] -> ([[Int]] -> [[Int]])) -> ([String] -> [[Int]]) -> ([String] -> [[Int]])
-- (<*>) ((<$>) (:) (p1)) :: ([String] -> [[Int]]) -> [String] -> [[Int]]

-- (p2)                        :: [String] -> [[Int]]
-- (<*>) ((<$>) (:) (p1)) (p2) :: [String] -> [[Int]]


