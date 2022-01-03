{-# LANGUAGE TemplateHaskell #-}

module Day23 where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Data.Maybe
import Control.Monad
import Debug.Trace (trace)

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
startingState = Map.fromList $ (points `zip` repeat Nothing) <> startingPoints where
    points = zip [0..10] (repeat 0) <> [(2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2)]
    startingPoints = [ ((2,1), Just PlayerB), ((2,2), Just PlayerA)
                     , ((4,1), Just PlayerC), ((4,2), Just PlayerD)
                     , ((6,1), Just PlayerB), ((6,2), Just PlayerC)
                     , ((8,1), Just PlayerD), ((8,2), Just PlayerA) ]

winningState = Map.fromList $ (points `zip` repeat Nothing) <> startingPoints where
    points = zip [0..10] (repeat 0) <> [(2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2)]
    startingPoints = [ ((2,1), Just PlayerA), ((2,2), Just PlayerA)
                     , ((4,1), Just PlayerB), ((4,2), Just PlayerB)
                     , ((6,1), Just PlayerC), ((6,2), Just PlayerC)
                     , ((8,1), Just PlayerD), ((8,2), Just PlayerD) ]


contrived = Map.fromList $ (points `zip` repeat Nothing) <> startingPoints where
    points = zip [0..10] (repeat 0) <> [(2,1),(2,2),(4,1),(4,2),(6,1),(6,2),(8,1),(8,2)]
    startingPoints = [ ((6,1), Just PlayerA), ((2,2), Just PlayerA)
                     , ((0,0), Just PlayerC), ((1,0), Just PlayerC)
                     , ((0,3), Just PlayerB), ((0,4), Just PlayerB)
                     , ((8,1), Just PlayerD), ((8,2), Just PlayerD) ]

day23a :: IO ()
day23a = do
    let results = iterate expand (Map.singleton contrived 0)
    let r2 = results `zip` drop 1 results
    let fix = takeWhile (uncurry (/=)) r2
    mapM_ print fix
    --let firstSuccess = head . dropWhile (not . Map.member winningState) $ results
    print $ Map.lookup winningState (fst . head $ fix)

day23b = undefined

tryApplyMove :: GameState -> PlayerMove -> Maybe GameState
tryApplyMove gs move = do
    maybeCurrentOccupant <- Map.lookup (move^.start) (gs^.board)
    currentOccupant <- maybeCurrentOccupant
    destinationOccupant <- Map.lookup (move^.end) (gs^.board)
    when (currentOccupant == PlayerB && (move^.start) == (4,1)) $
        trace (show (move^.end,destinationOccupant)) return ()
    guard $ not (inPlace (move^.start) currentOccupant) || (inPlace (move^.start) currentOccupant && inPlace (move^.end) currentOccupant)
    guard $ isNothing destinationOccupant
    let currentConstrained = isLetterRestrictionViolation (move^.start) currentOccupant
    let destConstrained = isLetterRestrictionViolation (move^.end) currentOccupant
    guard $ not destConstrained || (currentConstrained && destConstrained)
    return $ gs & board %~ Map.insert (move^.start) Nothing
                & board %~ Map.insert (move^.end) (Just currentOccupant)
                & totalCost +~ moveCost currentOccupant

moveCost :: PlayerLetter -> Integer
moveCost PlayerA = 1
moveCost PlayerB = 10
moveCost PlayerC = 100
moveCost PlayerD = 1000

isLetterRestrictionViolation :: BoardPosition -> PlayerLetter -> Bool
isLetterRestrictionViolation (_,0) _ = False
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


inPlace :: BoardPosition -> PlayerLetter -> Bool
inPlace (2,1) PlayerA = True
inPlace (2,2) PlayerA = True
inPlace (4,1) PlayerB = True
inPlace (4,2) PlayerB = True
inPlace (6,1) PlayerC = True
inPlace (6,2) PlayerC = True
inPlace (8,1) PlayerD = True
inPlace (8,2) PlayerD = True
inPlace _ _ = False

possibleTransitions :: GameState -> [GameState]
possibleTransitions gs = do
    p@(x,y) <- Map.keys (gs^.board)
    target <- [ (x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    maybeToList $ tryApplyMove gs (PlayerMove p target)

expand :: Map GameBoardState Integer -> Map GameBoardState Integer
expand curr = Map.unionWith min curr (Map.fromList next) where
    next =
        [ (g^.board, g^.totalCost)
        | g <- concatMap (possibleTransitions . uncurry GameState) . Map.toList $ curr ]