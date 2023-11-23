{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (digitToInt)
import Utils.List (chunksOf)
import Data.List (minimumBy)
import Data.Ord (comparing)

readDigits :: String -> [Int]
readDigits = map digitToInt

layeredBy :: Int -> Int -> [a] -> [[a]]
layeredBy w h = chunksOf (w*h)

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

fewestOf :: Eq a => a -> [[a]] -> [a]
fewestOf n = minimumBy (comparing (count n))

onesTimesTwos :: [Int] -> Int
onesTimesTwos xs = count 1 xs * count 2 xs

solution :: String -> String
solution = show . onesTimesTwos . fewestOf 0 . layeredBy 25 6 . readDigits

main :: IO ()
main = apply solution
