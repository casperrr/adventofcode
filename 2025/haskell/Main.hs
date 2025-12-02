module Main where
import Day1 (day1, day1')
import Day2

main :: IO ()
main = do
    putStrLn "===== Day 1 ====="
    d1p1 <- day1
    putStrLn $ "Part 1: " ++ show d1p1
    d1p2 <- day1'
    putStrLn $ "Part 2: " ++ show d1p2
    putStrLn "===== Day 2 ====="