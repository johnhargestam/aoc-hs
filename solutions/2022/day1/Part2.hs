{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (sortBy)
import Utils (descending)

top :: Ord a => Int -> [a] -> [a]
top n = take n . sortBy descending 

solution :: String -> String
solution = show . sum . top 3 . calorieSums

main :: IO ()
main = solve solution
