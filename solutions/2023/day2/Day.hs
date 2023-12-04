{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.List (find)
import Utils.Parser

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day2/sample"

data Round = Round { red :: Int, green :: Int, blue :: Int } deriving Show

data Game = Game { gameId :: Int, rounds :: [Round] } deriving Show

colorAmountP :: ReadP (Int, String)
colorAmountP = do
  n <- read <$> many1 digit
  skipSpaces
  clr <- many1 letter
  return (n, clr)

colorAmount :: String -> [(Int, String)] -> Int
colorAmount clr = maybe 0 fst . find isColor
  where isColor = (== clr) . snd

roundP :: ReadP Round
roundP = do
  colors <- sepBy1 colorAmountP (string ", ")
  return Round { red   = colorAmount "red" colors,
                 green = colorAmount "green" colors,
                 blue  = colorAmount "blue" colors }

gameP :: ReadP Game
gameP = do
  _ <- string "Game "
  i <- read <$> many1 digit
  _ <- string ": "
  rs <- sepBy1 roundP (string "; ")
  eof
  return Game { gameId = i, rounds = rs}
