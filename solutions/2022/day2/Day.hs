{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

solve :: (String -> String) -> IO ()
solve = evaluate "solutions/2022/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2022/day2/sample"

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

readShape :: Char -> Maybe Shape
readShape 'A' = Just Rock
readShape 'X' = Just Rock
readShape 'B' = Just Paper
readShape 'Y' = Just Paper
readShape 'C' = Just Scissors
readShape 'Z' = Just Scissors
readShape  _  = Nothing
