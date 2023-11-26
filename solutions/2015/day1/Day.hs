{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2015/day1/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2015/day1/sample"

go :: Char -> Int -> Int
go '(' n = n + 1
go ')' n = n - 1
go _   _ = undefined
