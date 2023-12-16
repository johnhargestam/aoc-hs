{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Text.Parsec.String (Parser)
import Text.Parsec (many1, oneOf)
import Utils.Parsec (sepBy1, space, digits)
import Text.Parsec.Char (char)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day12/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day12/sample"

data Springs = Springs { row :: String, groups :: [Int] }
  deriving Show

springsRowP :: Parser Springs
springsRowP = do
  xs <- many1 (oneOf "?.#")
  space
  gs <- map read <$> sepBy1 digits (char ',')
  return (Springs xs gs)
