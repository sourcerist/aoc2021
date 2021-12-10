module Day10 where

import Control.Lens
import Data.Foldable
import Data.List

day10a = do
    lst <- lines <$> readFile "./data/10.txt"
    let results = (`evalRemaining` []) <$> lst
    let scores = score1 <$> results ^.. each . _1
    print $ sum scores


day10b = do
    lst <- lines <$> readFile "./data/10.txt"
    let results = (`evalRemaining` []) <$> lst
    let scores = score2 <$> results ^.. each . filtered (null . fst) . _2
    let middle = (!! (length scores `div` 2)) . sort $ scores
    print middle

evalRemaining [] [] = ([], [])
evalRemaining [] ys = ([], ys)
evalRemaining ('(':xs) ys = evalRemaining xs (')':ys)
evalRemaining ('[':xs) ys = evalRemaining xs (']':ys)
evalRemaining ('<':xs) ys = evalRemaining xs ('>':ys)
evalRemaining ('{':xs) ys = evalRemaining xs ('}':ys)
evalRemaining (x:xs) ys
    | [x] == take 1 ys = evalRemaining xs (tail ys)
    | otherwise = (x:xs, ys)

score1 (')':_) = 3
score1 (']':_) = 57
score1 ('}':_) = 1197
score1 ('>':_) = 25137
score1 _ = 0

score2 :: String -> Integer
score2 = foldl' (\acc x -> acc * 5 + f x) 0 where
    f ')' = 1
    f ']' = 2
    f '}' = 3
    f '>' = 4
    f c = error $ "invalid char " <> [c]