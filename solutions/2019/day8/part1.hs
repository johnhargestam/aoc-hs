{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (minimumBy)
import Data.Ord (comparing)

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

fewestOf :: Eq a => a -> [[a]] -> [a]
fewestOf n = minimumBy (comparing (count n))

onesTimesTwos :: [Int] -> Int
onesTimesTwos xs = count 1 xs * count 2 xs

solution :: String -> String
solution = show . onesTimesTwos . fewestOf 0 . layers 25 6 . readDigits

main :: IO ()
main = apply solution
