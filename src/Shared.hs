module Shared where

import Data.Map (Map, (!))
import qualified Data.Map as Map

type Position = (Integer,Integer)

readMatrix :: String -> Map Position Integer
readMatrix str = Map.fromList $ [((x,y), read [v]) | (x,row) <- zip [0..] (lines str), (y,v) <- zip [0..] row]

showGrid :: Map Position Integer -> String 
showGrid m = unlines $ fmap rows [0..rowMax] where
    rows i = concatMap (show . (m !)) [(i,x) | x <- [0..colMax]]
    rowMax = maximum . fmap fst . Map.keys $ m
    colMax = maximum . fmap snd . Map.keys $ m