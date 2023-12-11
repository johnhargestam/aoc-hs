{-# OPTIONS_GHC -Wall #-}

import Day

difference :: [Int] -> Int
difference xs = maximum xs - minimum xs

checksum :: [[Int]] -> Int
checksum = sum . map difference

solution :: String -> String
solution = show . checksum . map numbers . lines

main :: IO ()
main = apply solution
