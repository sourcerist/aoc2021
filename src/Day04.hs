{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Day04 where

import System.IO
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Lens
import Data.List (unfoldr)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Control.Monad.State
import Debug.Trace


type GameBoard = Map (Integer,Integer) Integer

data BingoState = BingoState
    { _called :: Set Integer
    , _boards :: [GameBoard]}
    deriving (Eq, Ord, Show)
makeLenses ''BingoState

buildGameBoards :: [String] -> [GameBoard]
buildGameBoards = readToEnd . filter (/= "") where
    readToEnd = unfoldr (\xs -> if length xs >= 5 then Just (buildGameBoard (take 5 xs), drop 5 xs) else Nothing)
    buildGameBoard = Map.fromList . concat . zipWith buildRows [1..]
    buildRows rowNum line = zipWith (\x i -> ((rowNum, i), x)) (fmap read . words $ line) [1..]

day4a = do
    nums:rest <- lines <$> readFile "./data/4.txt"
    let bingoState = BingoState Set.empty  (buildGameBoards rest)
    let callNums = fmap read . splitOn "," $ nums
    let result = evalState (runUntilWinner callNums) bingoState
    print result

day4b = do
    nums:rest <- lines <$> readFile "./data/4.txt"
    let bingoState = BingoState Set.empty  (buildGameBoards rest)
    let callNums = fmap read . splitOn "," $ nums
    let result = evalState (runUntilLoser callNums) bingoState
    print result

winningSets :: [[(Integer, Integer)]]
winningSets = cols <> fmap (fmap swap) cols where
    cols = fmap (\r -> zip (repeat r) [1..5]) [1..5]

isWinner :: Set Integer -> GameBoard -> Bool
isWinner called board = any isWinner' winningSets  where
    isWinner' line = all (`Set.member` called) (fmap (board !) line)

calculateScore :: Set Integer -> GameBoard -> Integer
calculateScore called = sum . (`Set.difference` called) . Set.fromList . fmap snd . Map.toList

runUntilWinner :: [Integer] -> State BingoState Integer
runUntilWinner [] = do
    curr <- get
    error $ "no winner!  state = " <> show curr
runUntilWinner (x:xs) = do
    s <- gets (called %~ Set.insert x)
    let winners = filter (isWinner (s^.called)) (s^.boards)
    put s
    if null winners then runUntilWinner xs
    else return . (*x) . calculateScore (s^.called) . head $ winners


runUntilLoser :: [Integer] -> State BingoState Integer
runUntilLoser [] = do
    curr <- get
    error $ "no loser!  state = " <> show curr
runUntilLoser (x:xs) = do
    s <- gets (called %~ Set.insert x)
    let losers = filter (not . isWinner (s^.called)) (s^.boards)
    if null losers then return . (*x) . calculateScore (s^.called) . head $ (s^.boards)
    else do
        put $ set boards losers s
        runUntilLoser xs

