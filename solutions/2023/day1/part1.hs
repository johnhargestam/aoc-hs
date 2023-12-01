{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (isDigit)

value :: String -> Int
value = read . firstLast . filter isDigit
  where firstLast s = [head s, last s]

solution :: String -> String
solution = show . sum . map value . lines

main :: IO ()
main = apply solution
