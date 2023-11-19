{-# OPTIONS_GHC -Wall #-}

import Day

calculate :: Int -> Int
calculate x = x `div` 3 - 2

solution :: String -> String
solution = show . sum . map (calculate . read) . lines

main :: IO ()
main = apply solution
