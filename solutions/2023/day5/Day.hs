{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Text.Parsec.String (Parser)
import Text.Parsec (endBy1, eof, sepBy1)
import Text.Parsec.Char (newline)
import Utils.Parsec (sepByN, space, digits, letters, parseF, string)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day5/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day5/sample"

data Map = Map { source :: Int, destination :: Int, size :: Int }
  deriving Show

data Converter = Converter { from :: String, to :: String, maps :: [Map] }
  deriving Show

data Almanac = Almanac { seeds :: [Int], converters :: [Converter] }
  deriving Show

mapP :: Parser Map
mapP = do 
  [a,b,c] <- map read <$> sepByN 3 digits space
  return (Map {source = b, destination = a, size = c})

converterP :: Parser Converter
converterP = do
  f <- letters
  string "-to-"
  t <- letters
  string " map:\n"
  ms <- sepBy1 mapP newline
  return (Converter {from = f, to = t, maps = ms})



almanacP ::  Parser Almanac
almanacP = do
  string "seeds: "
  s <- map read <$> sepBy1 digits space
  string "\n\n"
  cs <- endBy1 converterP newline
  eof
  return (Almanac {seeds = s, converters = cs})

parseInput :: String -> Almanac
parseInput = parseF almanacP . (++"\n\n")
