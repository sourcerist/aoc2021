{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day08 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (unfoldr, sort)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Text.Parsec.String (Parser)
import Text.Parsec
import Control.Lens

type NumberSig = Set Char

data Entry = Entry
    { _key :: [NumberSig]
    , _output :: [NumberSig] }
    deriving (Eq, Ord, Show)
makeLenses ''Entry

day8a = do
    str <- readFile "./data/8.txt"
    let entries = case runParser parseEntries () "" str of
                    Left ex -> error . show $ ex
                    Right x -> x
    let values = length $ fmap evalEasyNum (entries ^.. traverse . output . each) ^.. traverse . _Just
    print values

day8b = do
    str <- readFile "./data/8.txt"
    let entries = case runParser parseEntries () "" str of
                    Left ex -> error . show $ ex
                    Right x -> x
    let decoded = fmap decodeEntry entries
    print $ sum decoded

evalEasyNum lst =
    case length lst of
        2 -> Just 1
        3 -> Just 7
        4 -> Just 4
        7 -> Just 8
        _ -> Nothing

parseNums :: Int -> Parser [String]
parseNums n  = (:) <$> many1 letter <*> count (n-1) (space *> many1 letter)

parseEntry = do
    s1 <- fmap Set.fromList <$> parseNums 10
    string " | "
    s2 <- fmap Set.fromList <$> parseNums 4
    return $ Entry s1 s2

parseEntries = parseEntry `sepBy1` newline <* eof

decodeEntry :: Entry -> Integer
decodeEntry entry = decode (entry^.output) where
    getWhere f = head $ entry ^. key ^.. folded . filtered f
    one = getWhere (\x -> length x == 2)
    seven = getWhere (\x -> length x == 3)
    four = getWhere (\x -> length x == 4)
    eight = getWhere (\x -> length x == 7)
    nine = getWhere (\x -> length x == 6 && Set.union seven four `Set.isSubsetOf` x)
    zero = getWhere (\x -> length x == 6 && x /= nine && seven `Set.isSubsetOf` x)
    six = getWhere (\x -> length x == 6 && x /= nine && x /= zero)
    five = getWhere (\x -> length x == 5 && length (six `Set.difference` x) == 1)
    two = getWhere (\x -> length x == 5 && length (five `Set.difference` x) == 2)
    three = getWhere (\x -> length x == 5 && x /= five && x /= two)
    m = Map.fromList $ zip [zero, one, two, three, four, five, six, seven, eight, nine] ['0'..]
    decode sigs = read . fmap (m !) $ sigs