module Main where

import Day01
import Day02
import Day03
import System.Environment

main :: IO ()
main = getArgs >>= run

run ["day1a"] = day1a
run ["day1b"] = day1b
run ["day2a"] = day2a
run ["day2b"] = day2b
run ["day3a"] = day3a
run ["day3b"] = day3b
run xs = putStrLn $ "Invalid input: " <> show xs
