{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (group)

say :: Int -> String -> String
say 0 s = s
say n s = say (n-1) $ concatMap count $ group s
  where count (x:xs) = show (length xs + 1) ++ [x]
        count []     = []

solution :: String -> String
solution = show . length . say 40

main :: IO ()
main = apply solution
