module Main where
import Day1 (day1, day1')
import Day2 (day2, day2')
import Day3 (day3, day3')
import Day4

main :: IO ()
main = do
    putStrLn "===== Day 1 ====="
    d1p1 <- day1
    putStrLn $ "Part 1: " ++ show d1p1
    d1p2 <- day1'
    putStrLn $ "Part 2: " ++ show d1p2
    putStrLn "===== Day 2 ====="
    d2p1 <- day2
    putStrLn $ "Part 1: " ++ show d2p1
    d2p2 <- day2'
    putStrLn $ "Part 2: " ++ show d2p2
    putStrLn "===== Day 3 ====="
    d3p1 <- day3
    putStrLn $ "Part 1: " ++ show d3p1
    d3p2 <- day3'
    putStrLn $ "Part 2: " ++ show d3p2