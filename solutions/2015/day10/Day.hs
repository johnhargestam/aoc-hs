{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.List (group)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2015/day10/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2015/day10/sample"

say :: Int -> String -> String
say 0 s = s
say n s = say (n-1) $ concatMap count $ group s
  where count (x:xs) = show (length xs + 1) ++ [x]
        count []     = []
