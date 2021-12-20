{-# LANGUAGE RecordWildCards #-}
module Day16 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Foldable
import Control.Monad.State.Strict
import Shared


day16a = do
    str <- getBits <$> readFile "./data/16.txt"
    let (p, (pos, remaining)) = runState packet (0, str)
    print $ countVersions p

day16b = do
    str <- getBits <$> readFile "./data/16.txt"
    let (p, (pos, remaining)) = runState packet (0, str)
    print $ evalPacket p

charMap = Map.fromList
    [ ('0', "0000")
    , ('1', "0001")
    , ('2', "0010")
    , ('3', "0011")
    , ('4', "0100")
    , ('5', "0101")
    , ('6', "0110")
    , ('7', "0111")
    , ('8', "1000")
    , ('9', "1001")
    , ('A', "1010")
    , ('B', "1011")
    , ('C', "1100")
    , ('D', "1101")
    , ('E', "1110")
    , ('F', "1111") ]

getBits :: String -> String
getBits = concatMap (charMap!)

data Packet = Packet
    { version :: Integer
    , typeId :: Integer
    , value :: PacketValue }
    deriving (Eq, Ord, Show)

data PacketValue
    = Literal Integer
    | Op [Packet]
    deriving (Eq, Ord, Show)

packet = do
    version <- readBits 3
    typeId <- readBits 3
    value <- if typeId == 4 then literal else operator
    return $ Packet version typeId value

packets = do
    keepGoing <- not <$> isEof
    if keepGoing then (:) <$> packet <*> packets
    else return []


literal = do
    pos <- readPos
    val <- binStringToInteger . concat <$> literalVals
    return $ Literal val

literalVals = do
    flag <- readBits 1
    val <- readChars 4
    if flag == 0 then return [val] else (val:) <$> literalVals

operator = do
    lengthIdFlag <- readBits 1
    if lengthIdFlag == 1 then do
        len <- readBits 11
        subpackets <- forM [1..len] (const packet)
        return $ Op subpackets
    else do
        len <- readBits 15
        inner <- readChars (fromIntegral len)
        let (subpackets,_) = runState packets (0,inner)
        return $ Op subpackets

readPackets i =
    if i == 0 then return []
    else do
        p <- packet
        (p:) <$> readPackets (i-1)

readBits i = binStringToInteger <$> readChars i

readChars :: Int -> State (Int, String) String
readChars i = do
    (pos, str) <- get
    if null str then error "unexpected EOF"
    else do
        put (pos + i, drop i str)
        return $ take i str

readPos = gets fst

isEof = gets (null . snd)

countVersions Packet{..} =
    case value of
        Literal l -> version
        Op xs -> version + (sum . fmap countVersions) xs

evalPacket Packet{..} =
    case value of
        Literal v -> v
        Op xs -> evalType typeId (fmap evalPacket xs)

evalType 0 = sum
evalType 1 = product
evalType 2 = minimum
evalType 3 = maximum
evalType 5 = \[x,y] -> if x > y then 1 else 0
evalType 6 = \[x,y] -> if x < y then 1 else 0
evalType 7 = \[x,y] -> if x == y then 1 else 0
evalType i = error $ "invalid type " <> show i
