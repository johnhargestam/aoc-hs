{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils.Parser

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day4/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day4/sample"

data Card = Card Int [Int] [Int] deriving Show

card :: ReadP Card
card = do
  _ <- string "Card"
  skipSpaces
  n <- read <$> many1 digit
  _ <- char ':'
  skipSpaces
  xs <- map read <$> sepBy (many1 digit) (many1 space)
  skipSpaces
  _ <- char '|'
  skipSpaces
  ys <- map read <$> sepBy (many1 digit) (many1 space)
  eof
  return (Card n xs ys)

wins :: Card -> Int
wins (Card _ xs ys) = length $ filter (`elem` xs) ys
