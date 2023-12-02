{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.Char (chr, ord)
import Data.List (group)
import Utils.Bool ((&&&))

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2015/day11/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2015/day11/sample"

allIncreasing :: Char -> Char -> Char -> Bool
allIncreasing x y z = ord x + 1 == ord y && ord x + 2 == ord z

hasIncreasing :: String -> Bool
hasIncreasing (x:y:z:zs) = allIncreasing x y z || hasIncreasing (y:z:zs)
hasIncreasing _ = False

isReadable :: String -> Bool
isReadable = not . any (`elem` "iol")

hasPairs :: String -> Bool
hasPairs = (>1) . length . filter ((>1) . length) . group

isValid :: String -> Bool
isValid = hasIncreasing &&& isReadable &&& hasPairs

letters :: Int -> String
letters = reverse . lettersRev
  where lettersRev n | (0, d) <- quotRem n 26 = [chr (97 + d)]
        lettersRev n | (r, d) <- quotRem n 26 = chr (97 + d) : lettersRev (r-1)

unletters :: String -> Int
unletters = subtract 1 . foldl (\a b -> a * 26 + (ord b - 96)) 0

nextPasswords :: String -> [String]
nextPasswords = map letters . iterate (+1) . unletters
