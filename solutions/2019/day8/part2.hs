{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (chunksOf)

override :: Int -> Int -> Int
override 2 b = b
override a _ = a

overlay :: [[Int]] -> [Int]
overlay = foldr1 (zipWith override)

showColors :: [Int] -> String
showColors = concatMap color
  where color 0 = "░"
        color _ = "█"

showLayer :: Int -> [Int] -> String
showLayer w = unlines . map showColors . chunksOf w

solution :: String -> String
solution = showLayer 25 . overlay . layers 25 6 . readDigits

main :: IO ()
main = apply solution
