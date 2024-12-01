{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Text.Parsec.String (Parser)
import Utils.Parsec (digits, parseF, spaces)

locationsP :: Parser (Int, Int)
locationsP = do
        n <- digits
        spaces
        m <- digits
        return (read n, read m)

parseLines :: String -> [(Int, Int)]
parseLines = map (parseF locationsP) . lines

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2024/day1/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2024/day1/sample"
