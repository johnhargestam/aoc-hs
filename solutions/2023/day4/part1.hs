{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parser (parse)

score :: Int -> Int
score 0 = 0
score n = 2^(n-1)

solution :: String -> String
solution = show . sum . map (score . wins . parse card) . lines

main :: IO ()
main = apply solution
