{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "template/input"

verify :: (String -> String) -> IO ()
verify = evaluate "template/sample"
