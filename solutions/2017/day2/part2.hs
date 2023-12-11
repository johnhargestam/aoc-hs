{-# OPTIONS_GHC -Wall #-}

import Day
 
divisible :: Int -> Int -> Bool
divisible a b = a `div` b * b == a 

pairs :: [Int] -> [(Int, Int)]
pairs xs = [(a, b) | a <- xs,
                     b <- xs,
                     a /= b]

checksum :: [[Int]] -> Int
checksum = sum . concatMap (map (uncurry div) . filter (uncurry divisible) . pairs)

solution :: String -> String
solution = show . checksum . map numbers . lines

main :: IO ()
main = apply solution
