{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Set (fromList, size)

allUnique :: String -> Bool
allUnique s = size (fromList s) == length s

fourUniqueIndex :: String -> Int
fourUniqueIndex = go 14
  where go i s
         | allUnique (take 14 s) = i
         | otherwise             = go (i+1) (drop 1 s)

solution :: String -> String
solution = show . fourUniqueIndex

main :: IO ()
main = apply solution
