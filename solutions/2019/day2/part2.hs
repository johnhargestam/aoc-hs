{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (splitEq)

seedNV :: [Int] -> (Int, Int) -> [Int]
seedNV (x:_:_:xs) (n,v) = x:n:v:xs
seedNV _ _              = undefined

outputNV :: [Int] -> (Int, Int) -> Int
outputNV xs = head . process . seedNV xs

findNV :: (Int -> Bool) -> [Int] -> (Int, Int)
findNV p xs = head $ filter (p . outputNV xs) nvs
  where nvs = [(n,v) | n <- [0..99], v <- [0..99]]

foldNV :: (Int, Int) -> Int
foldNV (n,v) = 100 * n + v

solution :: String -> String
solution = show . foldNV . findNV (== 19690720) . map read . splitEq ","

main :: IO ()
main = apply solution
