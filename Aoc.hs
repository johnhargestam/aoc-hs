{-# OPTIONS_GHC -Wall #-}

module Aoc where
  
import Utils (trim)

evaluate :: String -> (String -> String) -> IO ()
evaluate path sln = readFile path >>= putStrLn . sln . trim
