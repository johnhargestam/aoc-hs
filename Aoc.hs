{-# OPTIONS_GHC -Wall #-}

module Aoc where

evaluate :: String -> (String -> String) -> IO ()
evaluate path sln = readFile path >>= putStrLn . sln
