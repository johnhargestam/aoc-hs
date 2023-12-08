{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Text.Parsec.String (Parser)
import Text.Parsec (count, oneOf)
import Utils.Parsec (space, digits, newline, sepBy1)
import Data.List (group, sort, sortBy)
import Utils.List (descending)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day7/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day7/sample"

data Hand = Hand { cards :: [Char], bid :: Int }
  deriving Show

handP :: Parser Hand
handP = do
  cards <- count 5 (oneOf "AKQJT98765432")
  space
  bid <- read <$> digits
  return (Hand cards bid)

handsP :: Parser [Hand]
handsP = sepBy1 handP newline

sortedKinds :: Ord a => [a] -> [Int]
sortedKinds = sortBy descending . map length . group . sort

winnings :: [Hand] -> [Int]
winnings = zipWith (*) [1..] . map bid
