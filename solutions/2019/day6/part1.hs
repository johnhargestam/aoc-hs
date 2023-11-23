{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Tree (Tree (Node))

sumDepths :: Tree String -> Int
sumDepths = go 0
  where go d (Node _ []) = d
        go d (Node _ xs) = d + sum (map (go (d+1)) xs)

solution :: String -> String
solution = show . sumDepths . readTree

main :: IO ()
main = apply solution
