{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day6/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day6/sample"

data Race = Race { time :: Int, record :: Int }
  deriving Show

distance :: Int -> Int -> Int
distance mt ht = (mt - ht) * ht

wins :: Race -> Int
wins (Race mt rt) = length . filter (>rt) $ map (distance mt) [0..mt]
