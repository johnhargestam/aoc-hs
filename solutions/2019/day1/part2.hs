{-# OPTIONS_GHC -Wall #-}

import Day

fuelOfFuel :: Int -> Int
fuelOfFuel = sum . takeWhile (>0) . drop 1 . iterate fuel

solution :: String -> String
solution = show . sum . map (fuelOfFuel . read) . lines

main :: IO ()
main = apply solution
