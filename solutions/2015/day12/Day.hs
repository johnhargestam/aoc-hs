{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2015/day12/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2015/day12/sample"
