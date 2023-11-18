{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

solve :: (String -> String) -> IO ()
solve = evaluate "template/input"

verify :: (String -> String) -> IO ()
verify = evaluate "template/sample"
