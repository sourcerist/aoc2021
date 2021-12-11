{-# LANGUAGE TupleSections #-}
module Day09 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord
import Data.Function
import Data.List
import Control.Monad.State
import Debug.Trace
import Shared

day9a = do
    m <- readMatrix <$> readFile "./data/9.txt"
    let lowVals = fmap ((+1) . snd) . lowPoints $ m
    print $ sum lowVals
day9b = do
    m <- readMatrix <$> readFile "./data/9.txt"
    let lows = fmap fst . lowPoints $ m
    let basins = nub . fmap (flip evalState Set.empty . findBasin2 m . Set.singleton) $ lows
    let largestBasinSizes = fmap length . take 3 . sortBy (flip compare `on` length) $ basins
    print $ product largestBasinSizes

neighbors (x,y) m = catMaybes [ (p,) <$> Map.lookup p m | p <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] ]

lowPoints m = filter (\(p, v) -> all (> m!p) . fmap snd $ neighbors p m) (Map.toList m)

findBasin m acc p = eligible `Set.union` (Set.unions . Set.map (\(p',v) -> findBasin m acc' p')) eligible where
    eligible = Set.fromList . filter (\(p',v) -> v < 9 && not (Set.member p' acc)) . neighbors p $ m
    acc' = acc `Set.union` Set.map fst eligible

findBasin2 :: Map Position Integer -> Set Position -> State (Set Position) (Set (Position,Integer))
findBasin2 m ps = do
    prev <- get
    let edge = Set.fromList . fmap fst . filter (\(p,v) -> not (Set.member p prev) &&  v < 9) . concatMap (`neighbors` m) $ ps
    put $ prev `Set.union` edge
    let currentValues = Set.map (\p -> (p,m!p)) ps
    if null edge then return currentValues
    else Set.union currentValues <$> findBasin2 m edge