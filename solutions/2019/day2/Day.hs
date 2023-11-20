{-# OPTIONS_GHC -Wall #-}

module Day where

import Aoc (evaluate)
import Utils.List (replace)

apply :: (String -> String) -> IO ()
apply = evaluate "solutions/2019/day2/input"

verify :: (String -> String) -> IO ()
verify = evaluate "solutions/2019/day2/sample"

add :: Int -> Int -> Int -> [Int] -> [Int]
add x y z xs = replace z ((xs !! x) + (xs !! y)) xs

mul :: Int -> Int -> Int -> [Int] -> [Int]
mul x y z xs = replace z ((xs !! x) * (xs !! y)) xs

process :: [Int] -> [Int]
process xs = go 0 xs xs
  where go i (1:x:y:z:_) ys = next (i+4) $ add x y z ys
        go i (2:x:y:z:_) ys = next (i+4) $ mul x y z ys
        go _ (99:_)      ys = ys
        go _ _           _  = undefined
        next i zs = go i (drop i zs) zs
