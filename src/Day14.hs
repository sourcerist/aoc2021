{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Day14 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Shared
import Text.Parsec.String (Parser)
import Text.Parsec hiding (State)
import Data.Char (isSpace)
import Data.List (sort, group)
import Data.Ord
import Data.Function (on)
import Debug.Trace
import Control.Monad.State


day14a = do
    (inParam, m) <- readInput <$> readFile "./data/14.txt"
    let counts = Map.toList $ evalState (expandS m inParam 10) Map.empty
    let maxCount = maximumBy (compare `on` snd) counts
    let minCount = minimumBy (compare `on` snd) counts
    print $ snd maxCount - snd minCount


day14b = do
    (inParam, m) <- readInput <$> readFile "./data/14.txt"
    let counts = Map.toList $ evalState (expandS m inParam 40) Map.empty
    let maxCount = maximumBy (compare `on` snd) counts
    let minCount = minimumBy (compare `on` snd) counts
    print $ snd maxCount - snd minCount

readInput = parseOrError where
    parseOrError str = case runParser parseAll () "" str of
                         Left e -> error (show e)
                         Right val -> val
    parseAll = do
        inParam <- many1 letter
        ws
        entries <- entry `sepBy1` ws
        eof
        return (inParam, Map.fromList entries)
    ws = many $ satisfy isSpace
    entry = (\x y c -> ((x,y), c)) <$> letter <*> letter <* string " -> " <*> anyChar

expandS m str i = do
    let pairs = zip str (drop 1 str)
    combinedChildren <- if i == 0 then return Map.empty
                        else Map.unionsWith (+) <$> mapM (\(x,y) -> expandPairS m x y (i-1)) pairs
    let total = foldl' (\acc c -> Map.insertWith (+) c 1 acc) combinedChildren str
    return total

expandPairS m x y 0 = return $ Map.insert (m!(x,y)) 1 Map.empty
expandPairS m x y i = do
    memo <- gets (Map.lookup (x, y, i))
    case memo of
        Just cached -> return cached
        Nothing -> do
            let mid = m!(x,y)
            left <- expandPairS m x mid (i-1)
            right <- expandPairS m mid y (i-1)
            let combined = Map.insertWith (+) mid 1 $ Map.unionWith (+) left right
            modify (Map.insert (x,y,i) combined)
            return combined