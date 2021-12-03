module Day01 where

import System.IO

day1a = do
    str <- readFile "./data/1.txt"
    print $ (countIncreases . fmap read . words) str

day1b = do
    str <- readFile "./data/1.txt"
    print $ (countIncreases . fmap sum . slidingWindows . fmap read . words) str

countIncreases :: [Integer] -> Integer
countIncreases (x:y:xs) = curr + countIncreases (y:xs) where 
    curr = if x < y then 1 else 0
countIncreases _ = 0

slidingWindows [] = []
slidingWindows xs = take 3 xs : slidingWindows (drop 1 xs)