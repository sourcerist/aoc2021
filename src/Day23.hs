{-# LANGUAGE TemplateHaskell #-}

module Day23 where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Data.Maybe
import Control.Monad

type BoardPosition = (Integer, Integer)

data PlayerLetter = PlayerA | PlayerB | PlayerC | PlayerD
    deriving (Eq, Ord, Show, Bounded, Enum)
makePrisms ''PlayerLetter

data PlayerMove = PlayerMove
    { _start :: BoardPosition
    , _end :: BoardPosition }
    deriving (Eq, Ord, Show)
makeLenses ''PlayerMove

type GameBoardState = Map BoardPosition (Maybe PlayerLetter)

data GameState = GameState
    { _board :: GameBoardState
    , _totalCost :: Integer }
    deriving (Eq, Ord, Show)
makeLenses ''GameState

startingState :: GameBoardState
startingState = Map.fromList startingPoints `Map.union` Map.fromList (points `zip` repeat Nothing) where
    points = zip [0..10] (repeat 0) <> [(2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2)]
    startingPoints = [ ((2,1), Just PlayerB), ((2,2), Just PlayerA)
                     , ((4,1), Just PlayerC), ((4,2), Just PlayerD)
                     , ((6,1), Just PlayerB), ((6,2), Just PlayerC)
                     , ((8,1), Just PlayerD), ((8,2), Just PlayerA) ]

day23a :: IO ()
day23a = do
    print $ possibleTransitions (GameState startingState 0)

day23b = undefined

tryApplyMove :: GameState -> PlayerMove -> Maybe GameState
tryApplyMove gs move = do
    maybeCurrentOccupant <- Map.lookup (move^.start) (gs^.board)
    currentOccupant <- maybeCurrentOccupant
    destinationOccupant <- Map.lookup (move^.end) (gs^.board)
    guard $ isNothing destinationOccupant
    guard . not $ isLetterRestrictionViolation (move^.end) currentOccupant
    return $ gs & board %~ Map.insert (move^.start) Nothing
                & board %~ Map.insert (move^.end) (Just currentOccupant)
                & totalCost +~ moveCost currentOccupant

moveCost :: PlayerLetter -> Integer
moveCost = undefined

isLetterRestrictionViolation :: BoardPosition -> PlayerLetter -> Bool
isLetterRestrictionViolation (1,_) _ = False
isLetterRestrictionViolation (2,1) PlayerA = False
isLetterRestrictionViolation (2,1) _ = True
isLetterRestrictionViolation (2,2) PlayerA = False
isLetterRestrictionViolation (2,2) _ = False
isLetterRestrictionViolation (4,1) PlayerB = False
isLetterRestrictionViolation (4,1) _ = True
isLetterRestrictionViolation (4,2) PlayerB = False
isLetterRestrictionViolation (4,2) _ = False
isLetterRestrictionViolation (6,1) PlayerC = False
isLetterRestrictionViolation (6,1) _ = True
isLetterRestrictionViolation (6,2) PlayerC = False
isLetterRestrictionViolation (6,2) _ = False
isLetterRestrictionViolation (8,1) PlayerD = False
isLetterRestrictionViolation (8,1) _ = True
isLetterRestrictionViolation (8,2) PlayerD = False
isLetterRestrictionViolation (8,2) _ = False
isLetterRestrictionViolation _ _ = True

possibleTransitions :: GameState -> [GameState]
possibleTransitions gs = do
    p@(x,y) <- Map.keys (gs^.board)
    target <- [ (x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    maybeToList $ tryApplyMove gs (PlayerMove p target)

