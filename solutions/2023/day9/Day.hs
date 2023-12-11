{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils.List (mapAdjacent)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day9/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day9/sample"

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []      =  []
takeUntil p (x:xs)
        | p x       = [x]
        | otherwise = x : takeUntil p xs

diffs :: [Int] -> [Int]
diffs = mapAdjacent (flip (-))

numbers :: String -> [Int]
numbers = map read . words
