module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import System.Environment

main :: IO ()
main = getArgs >>= run

run ["day1a"] = day1a
run ["day1b"] = day1b
run ["day2a"] = day2a
run ["day2b"] = day2b
run ["day3a"] = day3a
run ["day3b"] = day3b
run ["day4a"] = day4a
run ["day4b"] = day4b
run ["day5a"] = day5a
run ["day5b"] = day5b
run ["day6a"] = day6a
run ["day6b"] = day6b
run xs = putStrLn $ "Invalid input: " <> show xs
