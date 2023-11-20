{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (splitEq)

twelvetwo :: [Int] -> [Int]
twelvetwo (x:_:_:xs) = x:12:2:xs
twelvetwo _          = undefined

solution :: String -> String
solution = show . head . process . twelvetwo . map read . splitEq ","

main :: IO ()
main = apply solution
