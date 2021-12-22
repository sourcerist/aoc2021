{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Day21 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Shared
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Control.Lens
import Control.Monad.State


data PlayerState = PlayerState
    { _score :: Integer
    , _position :: Integer }
    deriving (Eq, Ord, Show)
makeLenses ''PlayerState

data GameState = GameState
    { _players :: Map String PlayerState
    , _dieRolls :: [Integer]
    , _rollCount :: Integer }
makeLenses ''GameState

data Player = Player
    { _name :: String
    , _st :: PlayerState }
    deriving (Eq, Ord, Show)
makeLenses ''Player

type PlayerOrder = (Player, Player)


day21a = do
    let ps = Map.fromList [("P1", PlayerState 0 6), ("P2", PlayerState 0 1)]
    let die = concat . repeat $ [1..100]
    let (Just result) = execStateT (playUntilEnd "P1" "P2") (GameState ps die 0)
    print (_players result)

day21b = do
    let ps :: Map PlayerOrder Integer = Map.singleton (Player "P1" (PlayerState 0 6), Player "P2" (PlayerState 0 1)) 1
    let s = head . dropWhile (not . all hasWinner . Map.keys) . iterate  step $ ps
    let (p1s,p2s) = Map.partitionWithKey (\ps _ -> p1Winner ps) s
    print $ max (sum p1s)(sum p2s)

p1Winner :: PlayerOrder -> Bool
p1Winner (p1, p2) = (p1^.name == "P1" && p1^.st.score >= 21) || (p2^.name == "P1" && p2^.st.score >= 21)


rollDie :: MonadFail m => StateT GameState m Integer
rollDie = do
    (x:xs) <- use dieRolls
    dieRolls .= xs
    rollCount += 1
    return x

advance :: MonadFail m => String -> Integer -> StateT GameState m ()
advance player distance = zoom (players . ix player) $ do
    currPos <- use position
    let newPos = 1 + ((currPos - 1 + distance) `mod` 10)
    position .= newPos
    score += newPos

turn player = do
    roll <- sum <$> replicateM 3 rollDie
    advance player roll
    Just p <- use (players . at player)
    return $ p^.score >= 1000


playUntilEnd p1 p2 = do
    p1Result <- turn p1
    if p1Result then return ()
    else do
        p2Result <- turn p2
        if p2Result then return ()
        else playUntilEnd p1 p2


playRounds n p1 p2 = replicateM_ n round where
    round = do
        turn p1
        turn p2


hasWinner :: PlayerOrder -> Bool
hasWinner ps = any (>=21) (ps^..both.st.score)

updateWithRoll p rollTotal = p & st.score +~ newPos & set (st.position) newPos where
    newPos = 1 + ((p^.st.position - 1 + rollTotal) `mod` 10)

-- frequency of rolls
--
-- >>> foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty [ x+y+z | x <- [1..3], y <- [1..3], z <- [1..3]] 
-- fromList [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

singleStep :: PlayerOrder -> Integer -> Map PlayerOrder Integer
singleStep ps@(p1, p2) count = if hasWinner ps then Map.singleton ps count else newMap where
    newMap = Map.fromList $
        [ ((p2, updateWithRoll p1 rollTotal),count*n)
        | (rollTotal,n) <- [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)] ]

