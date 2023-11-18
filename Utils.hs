{-# OPTIONS_GHC -Wall #-}

module Utils where

import Data.List (tails, findIndex, isPrefixOf, dropWhileEnd)
import Data.Ord
import Data.Char (isSpace)

findTailIndex :: ([a] -> Bool) -> [a] -> Maybe Int
findTailIndex f ys = findIndex f (tails ys)

findListIndex :: Eq a => [a] -> [a] -> Maybe Int
findListIndex xs = findTailIndex (xs `isPrefixOf`)

splitEq :: Eq a => [a] -> [a] -> [[a]]
splitEq _  [] = []
splitEq [] xs = [xs]
splitEq ms xs = splitIndex (findListIndex ms xs)
  where splitIndex (Just i) = take i xs : splitEq ms (drop (i + length ms) xs)
        splitIndex Nothing  = [xs]

descending :: Ord a => a -> a -> Ordering
descending = comparing Down

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

singleMaybe :: [a] -> Maybe a
singleMaybe [x] = Just x
singleMaybe  _  = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ []         = []
chunksOf n xs
     | length xs >= n = take n xs : chunksOf n (drop n xs)
     | otherwise      = [xs]
