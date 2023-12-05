{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF)

score :: Int -> Int
score 0 = 0
score n = 2^(n-1)

solution :: String -> String
solution = show . sum . map (score . wins . parseF card) . lines

main :: IO ()
main = apply solution
