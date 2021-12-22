{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day20 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Shared
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)

day20a = do
    (algoInput : gridInput) <- lines <$> readFile "./data/20.txt"
    let algo = Map.fromList  $ [0..] `zip` fmap (=='#') algoInput
    let lighted :: Map (Integer, Integer) Char = readMatrix2 . formatForGrid . unlines $ gridInput
    let (_,lighted3) = (!! 2) . iterate (step algo) $ (0, lighted)
    print . length . Map.filter (=='1') $ lighted3

day20b = do
    (algoInput : gridInput) <- lines <$> readFile "./data/20.txt"
    let algo = Map.fromList  $ [0..] `zip` fmap (=='#') algoInput
    let lighted :: Map (Integer, Integer) Char = readMatrix2 . formatForGrid . unlines $ gridInput
    let (_,lighted50) = (!! 50) . iterate (step algo) $ (0, lighted)
    print . length . Map.filter (=='1') $ lighted50

surrounding (x,y) = [(x',y') | y' <- [(y-1)..(y+1)], x' <- [(x-1)..(x+1)]]

bitStringFor :: Map (Integer, Integer) Char -> Char -> (Integer,Integer) -> String
bitStringFor m defaultVal (x,y) = fmap (\p -> fromMaybe defaultVal (Map.lookup p m)) . surrounding $ (x,y)

isLighted :: Map Integer Bool -> Map Position Char -> Integer -> Position -> Bool
isLighted algo lighted stepNum p = algo ! (binStringToInteger . bitStringFor lighted defaultVal $ p) where
    defaultVal = boolToChar $ if odd stepNum then algo ! 0 else algo ! 511

boolToChar b = if b then '1' else '0'

step :: Map Integer Bool -> (Integer, Map (Integer,Integer) Char) -> (Integer, Map (Integer,Integer) Char)
step algo (stepNum, lighted) = (stepNum + 1, Map.fromList newLighted) where
    xMin = subtract 1 . minimum . fmap fst . Map.keys $ lighted
    yMin = subtract 1 . minimum . fmap snd . Map.keys $ lighted
    xMax = (+1) . maximum . fmap fst . Map.keys $ lighted
    yMax = (+1) . maximum . fmap snd . Map.keys$ lighted
    candidates = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]
    newLighted = (\p -> (p, if isLighted algo lighted stepNum p then '1' else '0')) <$> candidates


formatForGrid = fmap replaceChar where
    replaceChar '#' = '1'
    replaceChar '.' = '0'
    replaceChar c = c

unformatForGrid :: [Char] -> [Char]
unformatForGrid = fmap replaceChar where
    replaceChar '1' = '#'
    replaceChar '0' = '.'
    replaceChar c = c
