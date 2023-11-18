{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.Maybe
import Text.Read (readMaybe)
import Utils (split)

solve :: (String -> String) -> IO ()
solve = evaluate "solutions/2022/day1/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2022/day1/sample"

calories :: String -> [Int]
calories = mapMaybe readMaybe . lines

calorieSums :: String -> [Int]
calorieSums txt = map (sum . calories) (split "\n\n" txt)
