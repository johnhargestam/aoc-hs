{-# OPTIONS_GHC -Wall #-}

module Utils.List where

import Data.List (tails, findIndex, isPrefixOf)
import Data.Ord

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
