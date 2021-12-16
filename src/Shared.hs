module Shared where

import Data.Map (Map, (!))
import qualified Data.Map as Map

type Position = (Integer,Integer)

readMatrix :: String -> Map Position Integer
readMatrix str = Map.fromList $ [((x,y), read [v]) | (y,row) <- zip [0..] (lines str), (x,v) <- zip [0..] row]

showGrid :: Map Position Integer -> String 
showGrid m = unlines $ fmap rows [0..rowMax] where
    rows i = concatMap (show . (m !)) [(x,i) | x <- [0..colMax]]
    rowMax = maximum . fmap snd . Map.keys $ m
    colMax = maximum . fmap fst . Map.keys $ m