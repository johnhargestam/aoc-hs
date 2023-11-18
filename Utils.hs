{-# OPTIONS_GHC -Wall #-}

module Utils where

import Data.List (tails, findIndex, isPrefixOf)
import Data.Ord

findTailIndex :: ([a] -> Bool) -> [a] -> Maybe Int
findTailIndex f ys = findIndex f (tails ys)

findListIndex :: Eq a => [a] -> [a] -> Maybe Int
findListIndex xs = findTailIndex (xs `isPrefixOf`)

split :: Eq a => [a] -> [a] -> [[a]]
split _  [] = []
split [] xs = [xs]
split ms xs = splitIndex (findListIndex ms xs)
  where splitIndex (Just i) = take i xs : split ms (drop (i + length ms) xs)
        splitIndex Nothing  = [xs]

descending :: Ord a => a -> a -> Ordering
descending = comparing Down
