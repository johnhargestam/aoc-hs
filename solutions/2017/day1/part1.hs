{-# OPTIONS_GHC -Wall #-}

import Day

matches :: String -> [Int]
matches s = go s
  where go (x:y:zs) | x == y      = read [x] : go (y:zs)
        go [x]      | x == head s = [read [x]]
        go (_:xs)                 = go xs
        go []                     = []

solution :: String -> String
solution = show . sum . matches

main :: IO ()
main = apply solution
