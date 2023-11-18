{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Maybe (mapMaybe)

shapes :: String -> [Shape]
shapes = mapMaybe readShape

type Match = (Shape, Shape)

matchMaybe :: String -> Maybe Match
matchMaybe = match . shapes
  where match [x, y] = Just (x, y)
        match _ = Nothing

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

solution :: String -> String
solution = show . sum . map score . mapMaybe matchMaybe . lines

main :: IO ()
main = solve solution
