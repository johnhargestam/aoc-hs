{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (isInfixOf, group)
import Control.Monad (ap)

isWovel :: Char -> Bool
isWovel c = c `elem` "aeiou"

has3Vowels :: String -> Bool
has3Vowels = (3 ==)  . length . take 3 . filter isWovel

hasDoubleLetter :: String -> Bool
hasDoubleLetter = any ((> 1) . length) . group

notForbidden :: String -> Bool
notForbidden s = not $ any (`isInfixOf` s) forbidden
  where forbidden = ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice x = all (\f -> f x) [has3Vowels, hasDoubleLetter, notForbidden]

solution :: String -> String
solution = show . length . filter isNice . lines

main :: IO ()
main = apply solution
