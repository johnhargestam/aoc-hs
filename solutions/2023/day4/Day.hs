{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (mapMaybe)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day4/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day4/sample"

data Card = Card Int [Int] [Int]

instance Show Card where
  show (Card n _ _) = "Card " ++ show n

readInt :: String -> Maybe Int
readInt s | [(n,_)] <- reads s = Just n
          | otherwise          = Nothing

readId :: String -> Int
readId = read . last . words

readNumbers :: String -> ([Int], [Int])
readNumbers = bimap numbers numbers . break (== '|')
  where numbers = mapMaybe readInt . words

readCard :: String -> Card
readCard = toCard . bimap readId readNumbers . break (== ':')
  where toCard (i, (xs, ys)) = Card i xs ys

wins :: Card -> Int
wins (Card _ xs ys) = length $ filter (`elem` xs) ys
