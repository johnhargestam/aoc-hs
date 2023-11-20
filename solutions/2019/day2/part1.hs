{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (splitEq, replace)

add :: Int -> Int -> Int -> [Int] -> [Int]
add x y z xs = replace z ((xs !! x) + (xs !! y)) xs

mul :: Int -> Int -> Int -> [Int] -> [Int]
mul x y z xs = replace z ((xs !! x) * (xs !! y)) xs

process :: [Int] -> [Int]
process xs = go 0 xs xs
  where go i (1:x:y:z:_) ys = next (i+4) $ add x y z ys
        go i (2:x:y:z:_) ys = next (i+4) $ mul x y z ys
        go _ (99:_)      ys = ys
        go _ _           _  = undefined
        next i zs = go i (drop i zs) zs

twelvetwo :: [Int] -> [Int]
twelvetwo (x:_:_:xs) = x:12:2:xs
twelvetwo _          = undefined

solution :: String -> String
solution = show . head . process . twelvetwo . map read . splitEq ","

main :: IO ()
main = apply solution
