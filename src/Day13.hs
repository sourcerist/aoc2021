{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Day13 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Shared
import Text.Parsec.String (Parser)
import Text.Parsec
import Data.Char (isSpace)


day13a = do
    ops <- readOps <$> readFile "./data/13a.txt"
    let s = foldl' (\acc f -> f acc) Set.empty ops
    print $ length s

day13b = do
    ops <- readOps <$> readFile "./data/13b.txt"
    let s = foldl' (\acc f -> f acc) Set.empty ops
    let str = fmap (\c -> if c == '0' then ' ' else c) . showGrid . getGridFromSet $ s
    putStrLn str

getGridFromSet s = Map.fromList kvPairs where
    kvPairs = do 
        x <- [0..maxX]
        y <- [0..maxY]
        let v = if Set.member (x,y) s then 1 else 0
        return ((x,y), v)
    maxX = maximum . Set.map fst $ s
    maxY = maximum . Set.map snd $ s

readOps = parseOrError where
    parseOrError str = case runParser parseOps () "" str of
                         Left e -> error (show e)
                         Right val -> val
    parseOps = op `sepBy1` ws <* eof
    ws = many (satisfy isSpace)
    op = insertDot <|> try foldVertical <|> foldHorizontal
    insertDot = (\x y -> Set.insert (read x, read y)) <$> many1 digit <* char ',' <*> many1 digit
    foldVertical = foldVerticalAt . read <$> (string "fold along y=" *> many1 digit)
    foldHorizontal = foldHorizontalAt . read <$> (string "fold along x=" *> many1 digit)

foldVerticalAt :: Integer -> Set (Integer,Integer) -> Set (Integer,Integer)
foldVerticalAt foldY = Set.map f where
    f p@(x,y) 
        | y > foldY = (x, 2*foldY - y)
        | otherwise = p

foldHorizontalAt foldX = Set.map f where
    f p@(x,y) 
        | x > foldX = (2*foldX - x, y)
        | otherwise = p