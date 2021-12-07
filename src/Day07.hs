{-# LANGUAGE ScopedTypeVariables #-}
module Day07 where

import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Function (on)

day7a = do
    nums <- fmap read . splitOn "," <$> readFile "./data/7.txt"
    print $ cheapest nums movementCost
 
day7b :: IO ()
day7b = do
    nums <- fmap read . splitOn "," <$> readFile "./data/7.txt"
    print $ cheapest nums movementCost2

movementCost p1 p2 = abs (p2 - p1)

movementCost2 p1 p2 = (x * (x+1)) `div` 2 where x = abs (p2 - p1)

cheapest lst f = minimumBy (compare `on` snd) $ xs `zip` fmap cost xs where 
    xs = [minimum lst..maximum lst] 
    cost x = sum . fmap (f x) $ lst