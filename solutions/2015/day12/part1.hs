{-# OPTIONS_GHC -Wall #-}

import Day

integers :: String -> [Int]
integers [] = []
integers s | [(n, r)] <- reads s = n : integers r
           | otherwise           = integers $ drop 1 s

solution :: String -> String
solution = show . sum . integers

main :: IO ()
main = apply solution
