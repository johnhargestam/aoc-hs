{-# OPTIONS_GHC -Wall #-}

import Day

go :: Char -> Int -> Int
go '(' n = n + 1
go ')' n = n - 1
go _   _ = undefined

elevate :: [Char] -> Int
elevate = foldr go 0

solution :: String -> String
solution = unlines . map (show . elevate) . lines

main :: IO ()
main = apply solution
