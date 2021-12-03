{-# LANGUAGE TemplateHaskell #-}

module Day02 where

import Control.Lens

data Position = Position
    { _distance :: Integer
    , _depth :: Integer }
    deriving (Eq, Ord, Show)
makeLenses ''Position

getFunc :: [Char] -> Integer -> Maybe Position -> Maybe Position
getFunc "up" i x = x & _Just . depth -~ i 
getFunc "down" i x = x & _Just . depth +~ i 
getFunc "forward" i x = x & _Just . distance +~ i 
getFunc _ _ _ = Nothing

getMovements f = fmap ((\[movement, offset] -> f movement (read offset)) . words) . lines

day2a = do
    str <- readFile "./data/2.txt"
    let endPos = foldl (\acc f -> f acc) (Just $ Position 0 0) (getMovements getFunc str)
    print endPos

data SubmarineState = SubmarineState
    { _aim :: Integer
    , _position :: Position }
    deriving (Eq, Ord, Show)
makeLenses ''SubmarineState

getFunc2 "up" i x = x & _Just . aim -~ i
getFunc2 "down" i x = x & _Just . aim +~ i
getFunc2 "forward" i x = x & over _Just (\x' -> x' & position . depth +~ (x' ^. aim) * i)
                           & _Just . position . distance +~ i
getFunc2 _ _ _ = Nothing 

day2b = do
    str <- readFile "./data/2.txt"
    let endPos = foldl (\acc f -> f acc) (Just $ SubmarineState 0 (Position 0 0)) (getMovements getFunc2 str)
    print endPos