{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (singleMaybe)
import Data.Set (intersection, fromList, toList)
import Data.Maybe (mapMaybe)

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

allInBothHalves :: Ord a => ([a], [a]) -> [a]
allInBothHalves (xs, ys) = toList $ fromList xs `intersection` fromList ys

singleInBothHalves :: (Ord a) => ([a], [a]) -> Maybe a
singleInBothHalves = singleMaybe . allInBothHalves

misplacedItems :: [String] -> [Char]
misplacedItems = mapMaybe (singleInBothHalves . halves)

solution :: String -> String
solution = show . sum . priorities . misplacedItems . lines

main :: IO ()
main = apply solution
