module Day20 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Shared
import Control.Monad
import Data.Maybe (catMaybes, mapMaybe)
import qualified GHC.Arr as Set
import Debug.Trace

day20a = do
    (algoInput : gridInput) <- lines <$> readFile "./data/20.txt"
    let algo = Map.fromList  $ [0..] `zip` fmap (=='#') algoInput
    let lighted = Set.fromList . Map.keys . Map.filter (==1) . fmap (read . (:[])) . readMatrix2 . formatForGrid . unlines $ gridInput
    let lighted3 = (!! 2) . iterate (step algo) $ lighted
    putStrLn . unformatForGrid . showGrid . toDisplayGrid $ lighted3

-- >>> binStringToInteger "000000100"
-- 4

day20b = do
    str <- readFile "./data/20.txt"
    undefined

formatForGrid = fmap replaceChar where
    replaceChar '#' = '1'
    replaceChar '.' = '0'
    replaceChar c = c

unformatForGrid = fmap replaceChar where
    replaceChar '1' = '#'
    replaceChar '0' = '.'
    replaceChar c = c

surrounding (x,y) = [(x',y') | y' <- [(y-1)..(y+1)], x' <- [(x-1)..(x+1)]]

-- >>> bitStringFor (Set.fromList [(0,0),(0,1),(0,2),(1,2),(2,3),(2,4),(3,0),(3,4),(4,2),(4,4)]) (1,-1)
-- "000000100"

-- >>> [(x',y') | y' <- "abc", x' <- "xyz"]
-- [('x','a'),('y','a'),('z','a'),('x','b'),('y','b'),('z','b'),('x','c'),('y','c'),('z','c')]

bitStringFor s (x,y) = fmap (\p -> if Set.member p s then '1' else '0') . surrounding $ (x,y)

isLighted algo lighted p = algo ! (binStringToInteger . bitStringFor lighted $ p)

step algo lighted = Set.fromList newLighted where
    candidates = Set.fromList . concatMap surrounding $ lighted
    newLighted = mapMaybe (\p -> if isLighted algo lighted p then Just p else Nothing) . Set.toList $ candidates

toDisplayGrid s = Map.fromList [((x, y), v) |
                                x <- [0 .. xMax],
                                y <- [0 .. yMax],
                                let v = (if Set.member (x, y) s' then 1 else 0)] where
    xOffset = (*(-1)) . minimum . Set.map fst $ s
    yOffset = (*(-1)) . minimum . Set.map snd $ s
    s' = Set.map (\(x,y) -> (x+xOffset, y+yOffset)) s
    xMax = maximum . Set.map fst $ s'
    yMax = maximum . Set.map snd $ s'

