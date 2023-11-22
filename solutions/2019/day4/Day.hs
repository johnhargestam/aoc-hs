{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils.List (splitEq, mapAdjacent)
import Data.Char (digitToInt)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2019/day4/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2019/day4/sample"

digits :: Int -> [Int]
digits = map digitToInt . show

parseRange :: String -> [Int]
parseRange = between . map read . splitEq "-"
  where between [x,y] = [x..y]
        between _     = undefined

neverDecrease :: Ord a => [a] -> Bool
neverDecrease = and . mapAdjacent (<=)
