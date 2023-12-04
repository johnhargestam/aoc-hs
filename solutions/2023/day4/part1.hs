{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (mapMaybe)

data Card = Card Int [Int] [Int] deriving Show

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

score :: Int -> Int
score 0 = 0
score n = 2^(n-1)

wins :: Card -> Int
wins (Card _ xs ys) = length $ filter (`elem` xs) ys

solution :: String -> String
solution = show . sum . map (score . wins . readCard) . lines

main :: IO ()
main = apply solution
