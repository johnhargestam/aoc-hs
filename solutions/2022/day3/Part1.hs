{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (ord, isAsciiUpper, isAsciiLower)
import Utils (singleMaybe)
import Data.Set (intersection, fromList, toList)
import Data.Maybe

priority :: Char -> Int
priority c
       | isAsciiUpper c = ord c - 38
       | isAsciiLower c = ord c - 96
       | otherwise      = 0

priorities :: [Char] -> [Int]
priorities = map priority

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

allInBoth :: Ord a => ([a], [a]) -> [a]
allInBoth (xs, ys) = toList $ fromList xs `intersection` fromList ys

singleInBoth :: (Ord a) => ([a], [a]) -> Maybe a
singleInBoth = singleMaybe . allInBoth

misplacedItems :: [String] -> [Char]
misplacedItems = mapMaybe (singleInBoth . halves)

solution :: String -> String
solution = show . sum . priorities . misplacedItems . lines

main :: IO ()
main = solve solution
