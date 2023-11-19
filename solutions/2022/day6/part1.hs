{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Set (fromList, size)

isFourUnique :: Char -> Char -> Char -> Char -> Bool
isFourUnique a b c d = size (fromList [a,b,c,d]) == 4

fourUniqueIndex :: String -> Int
fourUniqueIndex = go 4
  where go i (a:b:c:d:es)
         | isFourUnique a b c d = i
         | otherwise            = go (i+1) (b:c:d:es)
        go _ _                  = error "invalid input"

solution :: String -> String
solution = show . fourUniqueIndex

main :: IO ()
main = apply solution
