{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils.List (splitEq)
import Data.Bifunctor (first)
import Data.List (find)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2023/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2023/day2/sample"

data Colors = Colors { red :: Int, green :: Int, blue :: Int } deriving Show

data Game = Game { gameId :: Int, colors :: [Colors] } deriving Show

readColorAmount :: String -> (Int, String)
readColorAmount = first read . tupled . words
  where tupled [x,y] = (x, y)
        tupled _     = undefined

colorAmount :: String -> [(Int, String)] -> Int
colorAmount color = maybe 0 fst . find isColor
  where isColor = (==color) . snd

readColors :: String -> Colors
readColors = toColors . map readColorAmount . splitEq ", "
  where toColors xs = Colors { red   = colorAmount "red" xs,
                               green = colorAmount "green" xs,
                               blue  = colorAmount "blue" xs }

readGame :: String -> Game
readGame = toGame . splitEq ": "
  where toGame [x,y] = Game (readId x) (map readColors $ splitEq "; " y)
        toGame _     = undefined
        readId = read . last . words
