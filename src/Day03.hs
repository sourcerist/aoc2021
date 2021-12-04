module Day03 where

import System.IO
import Data.List (transpose, foldl', group, sort)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)
import Control.Concurrent (yield)
import Debug.Trace

day3a = do
    bins <- words <$> readFile "./data/3.txt"
    let gamma = aggregateOn maximumBy bins
    let epsilon = aggregateOn minimumBy bins
    putStrLn $ "gamma: " <> gamma <> " (" <> (show . bin2dec) gamma <> ")"
    putStrLn $ "epsilon: " <> epsilon <> " (" <> (show . bin2dec) epsilon <> ")"


aggregateOn f = fmap (head . f (compare `on` length) . group . sort) . transpose

bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

bitFilterOxygen _ [y] = y
bitFilterOxygen i xs = bitFilterOxygen (i+1) filtered where
    filtered = if length mostCommon == length xs `div` 2 then [x | x <- xs, x!!i == '1']
               else [x | x <- xs, x!!i == head mostCommon]
    mostCommon = maximumBy (compare `on` length) . group . sort . fmap (!!i) $ xs
    
bitFilterCO2 _ [y] = y
bitFilterCO2 i xs = bitFilterCO2 (i+1) filtered where
    filtered = if length leastCommon == (length xs + 1) `div` 2 then [x | x <- xs, x!!i == '0']
               else [x | x <- xs, x!!i == head leastCommon]
    leastCommon = minimumBy (compare `on` length) . group . sort . fmap (!!i) $ xs

day3b = do
    bins <- words <$> readFile "./data/3.txt"
    let oxygen = bitFilterOxygen 0 bins
    let co2 = bitFilterCO2 0 bins
    putStrLn $ "oxygen: " <> oxygen <> " (" <> (show . bin2dec) oxygen <> ")"
    putStrLn $ "co2: " <> co2 <> " (" <> (show . bin2dec) co2 <> ")"