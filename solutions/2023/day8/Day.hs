{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Data.Maybe (mapMaybe)
import Data.Map ((!), Map, fromList)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, alphaNum)
import Text.Parsec (many1)
import Utils.Parsec (newline, string, sepBy1)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day8/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day8/sample"

data Direction = R | L
  deriving Show

type Node    = (String, (String, String))
type NodeMap = Map String (String, String)

charToDirection :: Char -> Maybe Direction
charToDirection 'L' = Just L
charToDirection 'R' = Just R
charToDirection  _  = Nothing

nodeP :: Parser Node
nodeP = do
  key <- many1 alphaNum
  string " = ("
  left <- many1 alphaNum
  string ", "
  right <- many1 alphaNum
  string ")"
  return (key, (left, right))

inputP :: Parser ([Direction], NodeMap)
inputP = do
  ds <- mapMaybe charToDirection <$> many1 (oneOf "LR")
  newline; newline
  ns <- sepBy1 nodeP newline
  return (ds, fromList ns)

turn :: Direction -> (a, a) -> a
turn L = fst
turn R = snd

pathFrom :: String -> ([Direction], NodeMap) -> [String]
pathFrom a (ds, m) = scanl navigate a (cycle ds)
  where navigate k d = turn d $ m ! k
