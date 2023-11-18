{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Maybe (mapMaybe)

readShape :: Char -> Maybe Shape
readShape 'A' = Just Rock
readShape 'X' = Just Rock
readShape 'B' = Just Paper
readShape 'Y' = Just Paper
readShape 'C' = Just Scissors
readShape 'Z' = Just Scissors
readShape  _  = Nothing

shapes :: String -> [Shape]
shapes = mapMaybe readShape

matchMaybe :: String -> Maybe Match
matchMaybe = match . shapes
  where match [x, y] = Just (x, y)
        match _      = Nothing

matches :: [String] -> [Match]
matches = mapMaybe matchMaybe

solution :: String -> String
solution = show . sum . map score . matches . lines

main :: IO ()
main = solve solution
