{-# OPTIONS_GHC -Wall #-}

module Utils where

import Data.List (tails, findIndex, isPrefixOf)

findSublistIndex :: ([a] -> Bool) -> [a] -> Maybe Int
findSublistIndex f ys = findIndex f (tails ys)

findSublistIndexEq :: Eq a => [a] -> [a] -> Maybe Int
findSublistIndexEq xs = findSublistIndex (xs `isPrefixOf`)

splitEq :: Eq a => [a] -> [a] -> [[a]]
splitEq [] xs = [xs]
splitEq ms xs = splitFirst (findSublistIndexEq ms xs)
  where splitFirst (Just i) = take i xs : splitEq ms (drop (i + length ms) xs)
        splitFirst Nothing  = splitEq [] xs
