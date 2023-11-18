{-# OPTIONS_GHC -Wall #-}

import Day
import Utils (chunksOf, singleMaybe)
import Data.Set (intersection, fromList, toList, empty, Set)
import Data.Maybe (mapMaybe)

allInChunk :: Ord a => [Set a] -> Set a
allInChunk (x : xs) = foldr intersection x xs
allInChunk []       = empty

singleInChunk :: Ord a => [[a]] -> Maybe a
singleInChunk = singleMaybe . toList . allInChunk . map fromList

badges :: [String] -> [Char]
badges = mapMaybe singleInChunk . chunksOf 3

solution :: String -> String
solution = show . sum . priorities . badges . lines

main :: IO ()
main = apply solution
