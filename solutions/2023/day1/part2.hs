{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (isPrefixOf, find)
import Data.Bifunctor (second)

digits :: [(String, String)]
digits = [("1", "one"), ("2", "two"), ("3", "three"),
          ("4", "four"), ("5", "five"), ("6", "six"),
          ("7", "seven"), ("8", "eight"), ("9", "nine")]

firstPrefix :: [(String, String)] -> String -> (String, String)
firstPrefix xs s = case find isAnyPrefix xs of
                     Just x  -> x
                     Nothing -> firstPrefix xs $ tail s
  where isAnyPrefix (x,y) = x `isPrefixOf` s || y `isPrefixOf` s

firstDigit :: String -> String
firstDigit = fst . firstPrefix digits

digitsRev :: [(String, String)]
digitsRev = map (second reverse) digits

lastDigit :: String -> String
lastDigit = fst . firstPrefix digitsRev . reverse

value :: String -> Int
value s = read $ firstDigit s ++ lastDigit s

solution :: String -> String
solution = show . sum . map value . lines

main :: IO ()
main = apply solution
