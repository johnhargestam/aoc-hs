{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Text.Parsec.String (Parser)
import Text.Parsec (spaces, sepBy, string, eof, endBy)
import Text.Parsec.Char (char)
import Utils.Parsec (digits)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day4/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day4/sample"

data Card = Card Int [Int] [Int] deriving Show

card :: Parser Card
card = do
  _ <- string "Card"
  spaces
  n <- read <$> digits
  _ <- char ':'
  spaces
  xs <- map read <$> endBy digits spaces
  _ <- char '|'
  spaces
  ys <- map read <$> sepBy digits spaces
  eof
  return (Card n xs ys)

wins :: Card -> Int
wins (Card _ xs ys) = length $ filter (`elem` xs) ys
