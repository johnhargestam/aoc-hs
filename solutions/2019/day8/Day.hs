{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.Char (digitToInt)
import Utils.List (chunksOf)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2019/day8/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2019/day8/sample"

readDigits :: String -> [Int]
readDigits = map digitToInt

layers :: Int -> Int -> [a] -> [[a]]
layers w h = chunksOf (w*h)
