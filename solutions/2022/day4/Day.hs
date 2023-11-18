{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils (splitEq)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2022/day4/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2022/day4/sample"

data Range = Range Int Int deriving (Show, Eq, Ord)

type Pair = (Range, Range)

readRange :: String -> Maybe Range
readRange = readRange' . mapMaybe readMaybe . splitEq "-"
  where readRange' [x,y] = Just (Range x y)
        readRange' _     = Nothing

readPair :: String -> Maybe Pair
readPair = readPair' . mapMaybe readRange . splitEq ","
  where readPair' [x,y] = Just (x, y)
        readPair' _     = Nothing

pairs :: [String] -> [Pair]
pairs = mapMaybe readPair
