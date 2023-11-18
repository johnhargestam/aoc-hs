{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2022/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2022/day2/sample"

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

type Match = (Shape, Shape)

isWin :: Match -> Bool
isWin (Rock,     Paper)    = True
isWin (Paper,    Scissors) = True
isWin (Scissors, Rock)     = True
isWin _                    = False

isDraw :: Match -> Bool
isDraw (x, y) = x == y

scoreMatch :: Match -> Int
scoreMatch m
         | isWin m = 6
         | isDraw m = 3
         | otherwise = 0

scoreShape :: Shape -> Int
scoreShape Rock     = 1
scoreShape Paper    = 2
scoreShape Scissors = 3

score :: Match -> Int
score m@(_, s) = scoreMatch m + scoreShape s
