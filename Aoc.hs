{-# OPTIONS_GHC -Wall #-}

module Aoc where
  
import Utils.String (trimEnd)

evaluate :: String -> (String -> String) -> IO ()
evaluate path sln = readFile path >>= putStrLn . sln . trimEnd
