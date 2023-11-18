{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

solve :: (String -> String) -> IO ()
solve = evaluate "solutions/2022/day3/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2022/day3/sample"
