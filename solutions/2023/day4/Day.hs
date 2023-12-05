{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Text.Parsec.String (Parser)
import Text.Parsec (many1, spaces, sepBy, string, eof, endBy)
import Text.Parsec.Char (char, digit, space)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day4/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day4/sample"

data Card = Card Int [Int] [Int] deriving Show

card :: Parser Card
card = do
  _ <- string "Card"
  spaces
  n <- read <$> many1 digit
  _ <- char ':'
  spaces
  xs <- map read <$> endBy (many1 digit) (many1 space)
  _ <- char '|'
  spaces
  ys <- map read <$> sepBy (many1 digit) (many1 space)
  eof
  return (Card n xs ys)

test :: Parser [Int]
test = do
  xs <- map read <$> endBy (many1 digit) (many1 space)
  --_ <- space
  _ <- char '|'
  --_ <- string " x"
  return xs

test2 :: Parser [Int]
test2 = do
  xs <- map read <$> sepBy (many1 digit) (many1 space)
  _ <- space
  _ <- char '|'
  --_ <- string " x"
  return xs

wins :: Card -> Int
wins (Card _ xs ys) = length $ filter (`elem` xs) ys
