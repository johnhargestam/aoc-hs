{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2024/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2024/day2/sample"
