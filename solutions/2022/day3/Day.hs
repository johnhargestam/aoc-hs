{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.Char (ord, isAsciiUpper, isAsciiLower)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2022/day3/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2022/day3/sample"

priority :: Char -> Int
priority c
       | isAsciiUpper c = ord c - 38
       | isAsciiLower c = ord c - 96
       | otherwise      = 0

priorities :: [Char] -> [Int]
priorities = map priority
