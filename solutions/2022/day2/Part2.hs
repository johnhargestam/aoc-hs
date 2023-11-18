{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Maybe (mapMaybe)

data Goal = Win | Draw | Loss

type Strategy = (Shape, Goal)

readShape :: Char -> Maybe Shape
readShape 'A' = Just Rock
readShape 'B' = Just Paper
readShape 'C' = Just Scissors
readShape  _  = Nothing

readGoal :: Char -> Maybe Goal
readGoal 'X' = Just Loss
readGoal 'Y' = Just Draw
readGoal 'Z' = Just Win
readGoal  _  = Nothing

strategyMaybe :: String -> Maybe Strategy
strategyMaybe [x, _, y] = strategy (readShape x) (readGoal y)
  where strategy (Just s) (Just g) = Just (s, g)
        strategy _ _               = Nothing
strategyMaybe _ = Nothing

win :: Shape -> Shape
win Rock     = Paper
win Paper    = Scissors
win Scissors = Rock

draw :: Shape -> Shape
draw = id

lose :: Shape -> Shape
lose Rock     = Scissors
lose Paper    = Rock
lose Scissors = Paper

execute :: Strategy -> Match
execute (s, g) = case g of
  Win ->  (s, win  s)
  Draw -> (s, draw s)
  Loss -> (s, lose s)

matches :: [String] -> [Match]
matches = mapMaybe (fmap execute . strategyMaybe)

solution :: String -> String
solution = show . sum . map score . matches . lines

main :: IO ()
main = solve solution
