{-# OPTIONS_GHC -Wall #-}

import Day

score :: Int -> Int
score 0 = 0
score n = 2^(n-1)

solution :: String -> String
solution = show . sum . map (score . wins . readCard) . lines

main :: IO ()
main = apply solution
