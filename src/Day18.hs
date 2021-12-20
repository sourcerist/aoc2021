{-# LANGUAGE FlexibleContexts #-}
module Day18 where

import Text.Parsec.String (Parser)
import Text.Parsec hiding (State)
import Data.Maybe
import Control.Monad.State
import Debug.Trace
import Data.List


day18a = do
    ts <- readInput <$> readFile "./data/18.txt"
    print . magnitude . sumTrees $ ts

day18b = do
    ts <- readInput <$> readFile "./data/18.txt"
    let bestCombo = maximum $
          [ magnitude x |
          l <- ts,
          r <- ts,
          l /= r,
          x <- [addTree l r, addTree r l] ]
    print bestCombo

data SFTree = Leaf Integer | Branch SFTree SFTree
    deriving (Eq, Ord, Show)

readInput :: String -> [SFTree]
readInput = parseOrError where
    parseOrError str = case runParser parseAll () "" str of
                         Left e -> error (show e)
                         Right val -> val
    parseAll = node `sepBy` newline <* eof
    leaf = Leaf . read <$> many1 digit
    node = leaf <|> branch
    branch = between (string "[") (string "]") (Branch <$> (node <* char ',') <*> node)

addNormalLeft i (Leaf x) = Leaf (i+x)
addNormalLeft 0 t = t
addNormalLeft i (Branch l r) = Branch (addNormalLeft i l) r

addNormalRight i (Leaf x) = Leaf (i+x)
addNormalRight 0 t = t
addNormalRight i (Branch l r) = Branch l (addNormalRight i r)

leafValue (Leaf n) = n
leafValue x = error $ "not a leaf: " <> show x

explode  t= (\(t',_,_) -> t') <$> explode' 0 t where
    explode' 4 (Branch l r) = Just (Leaf 0, leafValue l, leafValue r)
    explode' depth (Branch l r) =
        case explode' (depth + 1) l of
        Just (l', ln, rn) -> Just (Branch l' (addNormalLeft rn r), ln, 0)
        Nothing ->
            case explode' (depth + 1) r of
            Just (r', ln, rn) -> Just (Branch (addNormalRight ln l) r', 0, rn)
            Nothing -> Nothing
    explode' _ _ = Nothing

split (Leaf x) = if x > 9 then Just $ Branch (Leaf (x `div` 2)) (Leaf ((x + 1) `div` 2)) else Nothing
split (Branch l r) =
    case split l of
        Just l' -> Just (Branch l' r)
        Nothing ->
            case split r of
                Just r' -> Just (Branch l r')
                Nothing -> Nothing

reduce :: SFTree -> SFTree
reduce t =
    case explode t of
        Just t' -> reduce t'
        Nothing ->
            maybe t reduce (split t)

addTree l r = reduce $ Branch l r

sumTrees = foldl1' addTree

showTree (Branch l r) = "[" <> showTree l <> "," <> showTree r <> "]"
showTree (Leaf x) = show x

magnitude (Leaf n) = n
magnitude (Branch l r) = 3 * magnitude l + 2 * magnitude r
