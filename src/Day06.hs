{-# LANGUAGE TupleSections #-}
module Day06 where

import System.IO
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Foldable
import Data.List.Split

day6a = do
    str <- readFile "./data/6.txt"
    let initial = listToSumMap . fmap ((,1) . read) . splitOn "," $ str
    let result = getDayCount 80 initial
    print result

day6b = do
    str <- readFile "./data/6.txt"
    let initial = listToSumMap . fmap ((,1) . read) . splitOn "," $ str
    let result = getDayCount 256 initial
    print result

getDayCount x = (!! x) . fmap (sum . fmap snd . Map.toList) . iterate dailyIncrement

dailyIncrement :: Map Integer Integer -> Map Integer Integer
dailyIncrement = listToSumMap . concatMap f . Map.toList where
    f (0, count) = [(6, count), (8, count)]
    f (x, count) = [(x-1, count)]

listToSumMap = foldl' (\acc (k, v) -> Map.insertWith (+) k v acc) Map.empty