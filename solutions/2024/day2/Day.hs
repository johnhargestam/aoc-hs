{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

isSafeDelta :: (Ord a, Num a) => a -> Bool
isSafeDelta dt = dt >= 1 && dt <= 3

isSafeAsc :: (Ord a, Num a) => a -> a -> Bool
isSafeAsc x y = isSafeDelta $ y - x

isSafeDesc :: (Ord a, Num a) => a -> a -> Bool
isSafeDesc x y = isSafeDelta $ x - y

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2024/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2024/day2/sample"
