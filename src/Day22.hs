{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Day22 where

import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Lens
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (foldl')


data Toggle = On | Off
    deriving (Eq, Ord, Show)
makePrisms ''Toggle

data Cube = Cube
    { _xMin :: Integer
    , _xMax :: Integer
    , _yMin :: Integer
    , _yMax :: Integer
    , _zMin :: Integer
    , _zMax :: Integer }
    deriving (Eq, Ord, Show)
makeLenses ''Cube

data Command = Command
    { _toggle :: Toggle
    , _cube :: Cube }
    deriving (Eq, Ord, Show)
makeLenses ''Command

data Point3D = Point3D
    { _x :: Integer
    , _y :: Integer
    , _z :: Integer }
    deriving (Eq, Ord, Show)
makeLenses ''Point3D

day22a = do
    commands <- readInput <$> readFile "./data/22.txt"
    let validRange = Cube (-50) 50 (-50) 50 (-50) 50
    let validCommands = commands^..each.filtered (\x -> cubesOverlap validRange (x^.cube))
    let results = blinky validRange validCommands
    print $ length results

blinky zone commands = foldl' (\acc f -> f acc) Set.empty fs where
    fs = [ perform (Point3D x y z) command
         | x <- [(zone^.xMin)..(zone^.xMax)]
         , y <- [(zone^.yMin)..(zone^.yMax)]
         , z <- [(zone^.zMin)..(zone^.zMax)]
         , command <- commands ]

day22b = do
    str <- readFile "./data/22.txt"
    undefined

readInput = parseOrError where
    parseOrError str = case runParser parseAll () "" str of
                         Left e -> error (show e)
                         Right val -> val
    parseAll = command `sepBy1` newline <* eof
    command = do
        toggle <- try (string "on" $> On) <|> (string "off" $> Off)
        spaces
        (xMin, xMax) <- range 'x'
        char ','
        (yMin, yMax) <- range 'y'
        char ','
        (zMin, zMax) <- range 'z'
        return $ Command toggle (Cube xMin xMax yMin yMax zMin zMax)
    range c = do
        char c
        char '='
        min <- num
        string ".."
        max <- num
        return (min, max)
    num = do
        c <- option "" (string "-")
        i <- many1 digit
        return $ read (c <> i)

isBetween n b1 b2 = n >= b1 && n <= b2

pointInCube :: Point3D -> Command -> Bool
pointInCube p c = isBetween (p^.x) (c^.cube.xMin) (c^.cube.xMax) &&
                  isBetween (p^.y) (c^.cube.yMin) (c^.cube.yMax) &&
                  isBetween (p^.z) (c^.cube.zMin) (c^.cube.zMax)

cubesOverlap c1 c2 = dimensionOverlap (c1^.xMin) (c1^.xMax) (c2^.xMin) (c2^.xMax) &&
                     dimensionOverlap (c1^.yMin) (c1^.yMax) (c2^.yMin) (c2^.yMax) &&
                     dimensionOverlap (c1^.zMin) (c1^.zMax) (c2^.zMin) (c2^.zMax) where
    dimensionOverlap d1Min d1Max d2Min d2Max = isBetween d1Min d2Min d2Max ||
                                               isBetween d1Max d2Min d2Max ||
                                               isBetween d2Min d1Min d1Max ||
                                               isBetween d2Max d1Min d1Max

perform point command = 
    case (pointInCube point command, command^.toggle) of
        (True, On) -> Set.insert point
        (True, Off) -> Set.delete point
        (False, _) -> id

        

-- >>> pointInCube (Point3D 11 11 11) (Command {_toggle = On, _xMin = 10, _xMax = 12, _yMin = 10, _yMax = 12, _zMin = 10, _zMax = 12})
-- True
