{-# OPTIONS_GHC -Wall #-}

import Day

elevate :: [Char] -> Int
elevate = foldr go 0

solution :: String -> String
solution = unlines . map (show . elevate) . lines

main :: IO ()
main = apply solution
