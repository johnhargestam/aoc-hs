{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.List (find)
import Text.Parsec.String (Parser)
import Text.Parsec (many1, spaces, letter, sepBy1, string, eof)
import Utils.Parsec (digits)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day2/sample"

data Round = Round { red :: Int, green :: Int, blue :: Int } deriving Show

data Game = Game { gameId :: Int, rounds :: [Round] } deriving Show

colorAmountP :: Parser (Int, String)
colorAmountP = do
  n <- digits
  spaces
  clr <- many1 letter
  return (read n, clr)

colorAmount :: String -> [(Int, String)] -> Int
colorAmount clr = maybe 0 fst . find isColor
  where isColor = (== clr) . snd

roundP :: Parser Round
roundP = do
  colors <- sepBy1 colorAmountP (string ", ")
  return Round { red   = colorAmount "red" colors,
                 green = colorAmount "green" colors,
                 blue  = colorAmount "blue" colors }

gameP :: Parser Game
gameP = do
  _ <- string "Game "
  i <- read <$> digits
  _ <- string ": "
  rs <- sepBy1 roundP (string "; ")
  eof
  return Game { gameId = i, rounds = rs}
