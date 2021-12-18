module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import System.Environment ( getArgs )

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
run ["day7a"] = day7a
run ["day7b"] = day7b
run ["day8a"] = day8a
run ["day8b"] = day8b
run ["day9a"] = day9a
run ["day9b"] = day9b
run ["day10a"] = day10a
run ["day10b"] = day10b
run ["day11a"] = day11a
run ["day11b"] = day11b
run ["day12a"] = day12a
run ["day12b"] = day12b
run ["day13a"] = day13a
run ["day13b"] = day13b
run ["day14a"] = day14a
run ["day14b"] = day14b
run ["day15a"] = day15a
run ["day15b"] = day15b
run ["day16a"] = day16a
run ["day16b"] = day16b
run ["day17a"] = day17a
run ["day17b"] = day17b
run xs = putStrLn $ "Invalid input: " <> show xs
