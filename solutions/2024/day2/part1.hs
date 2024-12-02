{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (count)

allSafeBy :: (Ord a, Num a) => (a -> a -> Bool) -> [a] -> Bool
allSafeBy p [x, y] = p x y
allSafeBy p (x : y : zs) = p x y && allSafeBy p (y : zs)
allSafeBy _ _ = False

allSafe :: (Ord a, Num a) => [a] -> Bool
allSafe [x, y] = isSafeAsc x y || isSafeDesc x y
allSafe (x : y : zs)
        | isSafeAsc x y = allSafeBy isSafeAsc (y : zs)
        | isSafeDesc x y = allSafeBy isSafeDesc (y : zs)
        | otherwise = False
allSafe _ = False

numbers :: String -> [Int]
numbers = map read . words

solution :: String -> String
solution = show . count allSafe . map numbers . lines

main :: IO ()
main = apply solution
