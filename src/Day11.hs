
module Day11 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Shared
import Data.Maybe

day11a = do
    m <- readMatrix <$> readFile "./data/11.txt"
    let first100 = drop 1 . take 101 . iterate advance $ m
    print . sum . fmap countZeros $ first100

day11b = do
    m <- readMatrix <$> readFile "./data/11.txt"
    let rounds = [0..] `zip` iterate advance m
    let allFlashOnRound = fst . head . dropWhile (any (/= 0) . snd) $ rounds
    print allFlashOnRound

countZeros = length . Map.filter (==0)

advance = flash . fmap (+1)

flash :: Map Position Integer -> Map Position Integer
flash m = if null flashed then m else Map.unionWith const flashed next   where
    next = flash $ Map.unionWith const flashed adjustedNeighbors
    adjustedNeighbors = foldl' (flip (Map.adjust (+1))) m updates
    flashed = fmap (const 0) . Map.filter (>9) $ m
    updates =
            [ (x',y')
            | (x,y) <- Map.keys flashed
            , x' <- [x-1..x+1]
            , y' <- [y-1..y+1]
            , Map.member (x',y') m
            , not (Map.member (x',y') flashed)]


