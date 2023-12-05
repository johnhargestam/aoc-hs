{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (tails)
import Utils.Parsec (parseF)

scratch1 :: [Int] -> Int
scratch1 []     = 0
scratch1 (0:_)  = 1
scratch1 (1:ws) = 1 + scratch1 ws
scratch1 (w:ws) = 1 + scratchN w ws

scratchN :: Int -> [Int] -> Int
scratchN n = sum . map scratch1 . take n . tails

scratchOriginal :: [Int] -> Int
scratchOriginal ws = scratchN (length ws) ws

solution :: String -> String
solution = show . scratchOriginal . map (wins . parseF card) . lines

main :: IO ()
main = apply solution
