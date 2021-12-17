
module Day15 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Shared
import qualified Control.Lens as Map
import Data.Function (on)

day15a = do
    m <- readMatrix <$> readFile "./data/15.txt"
    let maxPosition = maximum . Map.keys $ m
    print $ findPathCost m (Set.singleton (0,0)) (Set.singleton (0, (0,0))) maxPosition

day15b = do
    m <- expandGrid . readMatrix <$> readFile "./data/15.txt"
    let maxPosition = maximum . Map.keys $ m
    print $ findPathCost m Set.empty (Set.singleton (0, (0,0))) maxPosition

type Grid = Map Position Integer

type VisitedSet = Set Position

type CostSet = Set (Integer, Position)

findPathCost :: Grid -> VisitedSet -> CostSet -> Position -> Integer
findPathCost grid visited priorityQueue destination = if (x,y) == destination then cost else next where
    Just (cheapest@(cost, pos@(x,y)), tmp) = filteredMinView visited priorityQueue
    newNeighbors = Set.fromList $
        [ (cost + (grid!p), p)
        | p <- [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
        , Map.member p grid
        , not $ Set.member p visited ]
    newQueue = tmp `Set.union` newNeighbors
    next = findPathCost grid (Set.insert pos visited) newQueue destination

filteredMinView visited priorityQueue = do
    v@((_,p), remaining) <- Set.minView priorityQueue
    if not (Set.member p visited) then return v else filteredMinView visited remaining

expandGrid :: Grid -> Grid
expandGrid grid = Map.fromList expanded where
    (xLen, yLen) = (\(x,y) -> (x+1,y+1)) . maximum . Map.keys $ grid
    expanded =
        [ ((x', y'), cost') |
          ((x, y), cost) <- Map.toList grid,
          nx <- [0 .. 4],
          let x' = x + nx * xLen,
          ny <- [0 .. 4],
          let y' = y + ny * yLen,
          let rawCost = cost + nx + ny,
          let cost' = if rawCost > 9 then 1 + (rawCost `mod` 10) else rawCost ]