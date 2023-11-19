{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (chunksOf, singleMaybe)
import Data.Set (intersection, fromList, toList, Set)
import Data.Maybe (mapMaybe)


allInChunk :: Ord a => [Set a] -> Set a
allInChunk = foldr1 intersection

singleInChunk :: Ord a => [[a]] -> Maybe a
singleInChunk = singleMaybe . toList . allInChunk . map fromList

badges :: [String] -> [Char]
badges = mapMaybe singleInChunk . chunksOf 3

solution :: String -> String
solution = show . sum . priorities . badges . lines

main :: IO ()
main = apply solution
