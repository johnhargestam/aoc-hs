{-# OPTIONS_GHC -Wall #-}

module Part1 where

import Day (solve)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Utils (splitEq)

calories :: String -> [Int]
calories = mapMaybe readMaybe . lines

solution :: String -> String
solution txt = show $ maximum $ map (sum . calories) (splitEq "\n\n" txt)

main :: IO ()
main = solve solution
