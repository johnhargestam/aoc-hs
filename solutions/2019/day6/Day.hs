{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils.List (splitEq)
import Data.Map (Map, fromListWith, findWithDefault)
import Data.Tree (Tree, unfoldTree)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2019/day6/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2019/day6/sample"

readKeyValue :: String -> (String, [String])
readKeyValue = keyValue . splitEq ")"
  where keyValue [x,y] = (x, [y])
        keyValue _     = undefined

toKeyValues :: [String] -> [(String, [String])]
toKeyValues = map readKeyValue

toMap :: [(String, [String])] -> Map String [String]
toMap = fromListWith (++)

toTree :: Map String [String] -> Tree String
toTree m = unfoldTree build "COM"
  where build k = (k, findWithDefault [] k m)

readTree :: String -> Tree String
readTree = toTree . toMap . toKeyValues . lines
