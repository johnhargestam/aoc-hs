{-# OPTIONS_GHC -Wall #-}

module Utils.List where

import Data.List (tails, findIndex, isPrefixOf, find)
import Data.Ord
import Data.Maybe (fromMaybe)

findTailIndex :: ([a] -> Bool) -> [a] -> Maybe Int
findTailIndex f ys = findIndex f (tails ys)

findListIndex :: Eq a => [a] -> [a] -> Maybe Int
findListIndex xs = findTailIndex (xs `isPrefixOf`)

splitEq :: Eq a => [a] -> [a] -> [[a]]
splitEq _  [] = []
splitEq [] xs = [xs]
splitEq ms xs = case findListIndex ms xs of
  (Just i) -> take i xs : splitEq ms (drop (i + length ms) xs)
  Nothing -> [xs]

descending :: Ord a => a -> a -> Ordering
descending = comparing Down

singleMaybe :: [a] -> Maybe a
singleMaybe [x] = Just x
singleMaybe  _  = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ []         = []
chunksOf n xs
     | length xs >= n = take n xs : chunksOf n (drop n xs)
     | otherwise      = [xs]

sieve :: Int -> [b] -> [b]
sieve n = map head . takeWhile (not . null) . iterate (drop n)

replace :: Int -> a -> [a] -> [a]
replace i x xs = ys ++ [x] ++ drop 1 zs
  where (ys,zs) = splitAt i xs

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs $ tail xs

findWithDefault :: a -> (a -> Bool) -> [a] -> a
findWithDefault x p = fromMaybe x . find p

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

countWhile :: (a -> Bool) -> [a] -> Int
countWhile p = go 0
  where go !n (x:xs) | p x = go (n+1) xs
        go !n _            = n
