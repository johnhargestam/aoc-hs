{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils.List (splitEq)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2015/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2015/day2/sample"

tuple3 :: [a] -> (a, a, a)
tuple3 [a,b,c] = (a, b, c)
tuple3 _       = undefined

readDimension :: String -> (Int, Int, Int)
readDimension = tuple3 . map read . splitEq "x"

requiredPaper :: (Int, Int, Int) -> Int
requiredPaper (l,w,h) = 2*lw + 2*wh + 2*hl + minimum [lw, wh, hl]
   where (lw, wh, hl) = (l*w, w*h, h*l)
