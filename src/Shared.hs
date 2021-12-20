module Shared where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Traversable (for)

type Position = (Integer,Integer)

readMatrix :: String -> Map Position Integer
readMatrix str = Map.fromList $ [((x,y), read [v]) | (y,row) <- zip [0..] (lines str), (x,v) <- zip [0..] row]

showGrid :: Map Position Integer -> String 
showGrid m = unlines $ fmap rows [0..rowMax] where
    rows i = concatMap (show . (m !)) [(x,i) | x <- [0..colMax]]
    rowMax = maximum . fmap snd . Map.keys $ m
    colMax = maximum . fmap fst . Map.keys $ m

readMatrix2 input = Map.fromList  $ concatMap readRow ([0..] `zip` (filter (not . null) . lines) input) where
    readRow (y,r) = fmap (\(x,v) -> ((x,y), v)) ([0..] `zip` r)


binStringToInteger :: String -> Integer
binStringToInteger = convert' . fmap (read . (:[])) . reverse where
    convert' [] = 0
    convert' (x : xs) = x + 2 * convert' xs