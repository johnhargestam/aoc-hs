{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2015/day3/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2015/day3/sample"

navigate :: (Int, Int) -> Char -> (Int, Int)
navigate (x,y) '>' = (x+1, y)
navigate (x,y) 'v' = (x, y-1)
navigate (x,y) '<' = (x-1, y)
navigate (x,y) '^' = (x, y+1)
navigate  _ _      = undefined

positions :: [Char] -> [(Int, Int)]
positions = scanl navigate (0,0)
