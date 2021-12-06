module Day05 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (unfoldr)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Data.Foldable
import Text.Parsec.String (Parser)
import Text.Parsec

type Point = (Integer, Integer)
type Positions = Map Point Integer


day5a = do
    str <- readFile "./data/5.txt"
    let lst = case runParser parseLines () "" str of
                  Left ex -> error . show $ ex
                  Right x -> x
    let finalMap = foldl' (\acc (p1,p2) -> addLine p1 p2 acc) Map.empty lst
    let overlaps = [(x,y) | ((x,y),count) <- Map.toList finalMap, count > 1]
    print $ length overlaps
    
day5b = do
    str <- readFile "./data/5.txt"
    let lst = case runParser parseLines () "" str of
                  Left ex -> error . show $ ex
                  Right x -> x
    let finalMap = foldl' (\acc (p1,p2) -> addLine2 p1 p2 acc) Map.empty lst
    let overlaps = [(x,y) | ((x,y),count) <- Map.toList finalMap, count > 1]
    print $ length overlaps

addLine :: Point -> Point -> Positions -> Positions
addLine (x1,y1) (x2,y2) m
    | x1 == x2 = foldl' (\acc y -> Map.insertWith (+) (x1,y) 1 acc) m [(min y1 y2)..(max y1 y2)]
    | y1 == y2 = foldl' (\acc x -> Map.insertWith (+) (x,y1) 1 acc) m [(min x1 x2)..(max x1 x2)]
    | otherwise = m

addLine2 :: Point -> Point -> Positions -> Positions
addLine2 (x1,y1) (x2, y2) m = foldl' (\acc p -> Map.insertWith (+) p 1 acc) m points where
    points = case (x1 `compare` x2, y1 `compare` y2) of
        (EQ, _) -> zip (repeat x1) [(min y1 y2)..(max y1 y2)]
        (_, EQ) -> zip [(min x1 x2)..(max x1 x2)] (repeat y1)
        (LT, LT) -> zip [x1..x2] [y1..y2]
        (GT, LT) -> zip (reverse [x2..x1]) [y1..y2]
        (LT, GT) -> zip [x1..x2] (reverse [y2..y1]) 
        (GT, GT) -> zip (reverse [x2..x1]) (reverse [y2..y1]) 

parsePoint :: Parser Point
parsePoint = do
    x <- read <$> many1 digit
    char ','
    y <- read <$> many1 digit
    return (x, y)

parseLine = do
    p1 <- parsePoint
    spaces
    string "->"
    spaces
    p2 <- parsePoint
    return (p1, p2)

parseLines = parseLine `sepBy1` newline <* eof
