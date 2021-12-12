{-# LANGUAGE TupleSections #-}

module Day12 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Shared
import Data.Maybe
import Text.Parsec.String (Parser)
import Text.Parsec
import Data.Char (isLower, isUpper)
import Data.Tuple
import Data.List (intercalate, nub, group, sort)

day12a = do
    s <- (\x -> Set.union x (Set.map swap x)) . readGraph <$> readFile "./data/12.txt"
    print . length $ search s Set.empty "start"

day12b = do
    s <- (\x -> Set.union x (Set.map swap x)) . readGraph <$> readFile "./data/12.txt"
    let m = Map.fromSet (const 0) s
    let results = nub $ search2 m [] "start"
    print $ length results

graphParser :: Parser (Set (String, String))
graphParser = Set.fromList <$> entry `sepBy1` newline <* eof where
    entry = (,) <$> many1 letter <* char '-' <*>  many1 letter

readGraph str = case runParser graphParser () "" str of
                Left err -> error . show $ err
                Right x -> x

getPath :: Set (String, String) -> String ->  [String]
getPath s node =
    case Set.toList . Set.filter ((==node) . fst) $ s of
        [] -> ["end"]
        xs -> node : getPath (Set.delete (head xs) s) (snd . head $ xs)


getPath2 :: Map (String, String) Int -> String ->  [String]
getPath2 m node =
    case Map.toList . Map.filterWithKey(\(l,r) i -> l == node) $ m of
        [] -> ["end"]
        ((l,r), 1):_ -> node : getPath2 (Map.delete (l,r) m) r
        ((l,r), i):_ -> node : getPath2 (Map.insert (l,r) (i-1) m) r

search :: Set (String, String) -> Set (String, String) -> String -> [Set (String,String)]
search s visited "end" = [visited]
search s visited node = concatMap (\next -> search s' (Set.insert (node, next) visited) next) connectedNodes where
    s' = if all isUpper node then s else Set.filter (\(l,r) -> r /= node) s
    connectedNodes = fmap snd . Set.toList . Set.filter (\v@(l,r) -> l == node) $ s

search2 :: Map (String, String) Int -> [String] -> String -> [[String]]
search2 m visited "end" = [reverse ("end":visited)]
search2 m visited node = concatMap (\next -> search2 (Map.adjust (+1) (node,next) m) (node:visited) next) eligibleNodes where
    eligibleNodes
        | not (any ((>1) . snd) lowerCaseNodeCounts) = allNeighbors
        | otherwise = notMoreThanOnceNeighbors
    lowerCaseNodeCounts =
        fmap (\x -> (head x, length x)) . group . sort . filter (\s -> all isLower s && s /= "start") $ (node:visited)
    allNeighbors = fmap snd . filter (\(l,r) -> l == node && r /= "start") . Map.keys $ m
    notMoreThanOnceNeighbors = filter (\x -> all isUpper x || notElem x (fmap fst lowerCaseNodeCounts)) allNeighbors

