{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day17 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Foldable
import Text.Parsec.String (Parser)
import Text.Parsec hiding (State)
import Data.Char (isSpace)
import Data.Function (on)
import Control.Monad.State
import Data.Maybe (isNothing, isJust, mapMaybe, catMaybes)


day17a = do
    (x1,x2,y1,y2) :: (Int, Int, Int, Int) <- readInput <$> readFile "./data/17.txt"
    print . maximum . fmap (\(_,_,bestY) -> bestY) $ findHits x1 x2 y1 y2


day17b = do
    (x1,x2,y1,y2) :: (Int, Int, Int, Int) <- readInput <$> readFile "./data/17.txt"
    print . length $ findAllHits x1 x2 y1 y2

readInput = parseOrError where
    parseOrError str = case runParser parseAll () "" str of
                         Left e -> error (show e)
                         Right val -> val
    parseAll = do
        string "target area: x="
        x1 <- num
        string ".."
        x2 <- num
        string ", y="
        y1 <- num
        string ".."
        y2 <- num
        eof
        return (x1,x2,y1,y2)
    num = do
        c <- option "" (string "-")
        i <- many1 digit
        return $ read (c <> i)


step (x, y, vx, vy) = (x+vx, y+vy, max 0 (vx-1), vy-1)

frames vx vy xMin xMax yMin yMax = takeWhile (\(_,y,_,_) -> y >= yMin) . iterate step $ (0, 0, vx, vy)

maxHeightIfHit vx vy xMin xMax yMin yMax = if any isHit states then Just (getMaxY states) else Nothing where
    states = frames vx vy xMin xMax yMin yMax
    isHit (x,y,_,_) = x >= xMin && x <= xMax && y >= yMin && y <= yMax
    getMaxY = maximum . fmap (\(_,y,_,_) -> y) 

xVelocityBounds xMin xMax = takeWhile (\x -> summation x <= xMax) . dropWhile (\x -> summation x < xMin) $ [0..] where
    summation x = (1 + x) * x `div` 2

findHits xMin xMax yMin yMax = [ (xv,yv,result) | xv <- [xvMin..xvMax], (yv,result) <- rangeFor xv ] where
    xvBounds = xVelocityBounds xMin xMax
    xvMin = minimum xvBounds
    xvMax = maximum xvBounds
    Just yvLower = fmap fst . head . dropWhile isNothing . fmap (boundWith xvMin) $ [yMin..]
    rangeFor xv = mapMaybe (boundWith xv) [yvLower..(2 * abs yMax)]
    boundWith xv yv = (yv,) <$> maxHeightIfHit xv yv xMin xMax yMin yMax

findAllHits xMin xMax yMin yMax = [ (xv,yv,result) | xv <- [xvMin..xvMax], (yv,result) <- rangeFor xv ] where
    xvBounds = xVelocityBounds xMin xMax
    xvMin = minimum xvBounds
    xvMax = xMax
    Just yvLower = fmap fst . head . dropWhile isNothing . fmap (boundWith xvMin) $ [yMin..]
    rangeFor xv = mapMaybe (boundWith xv) [yMin..(2 * abs yMax)]
    boundWith xv yv = (yv,) <$> maxHeightIfHit xv yv xMin xMax yMin yMax