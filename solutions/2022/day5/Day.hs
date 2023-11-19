{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2022/day5/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2022/day5/sample"

verify' :: (String -> String) -> IO ()
verify' = evaluate "solutions/2022/day5/sample2"
