{-# OPTIONS_GHC -Wall #-}

import Day

levels :: [Char] -> [Int]
levels = scanl (flip go) 0

position :: [Char] -> Int
position = length . takeWhile (-1/=). levels

solution :: String -> String
solution = unlines . map (show . position) . lines

main :: IO ()
main = apply solution
