{-# OPTIONS_GHC -Wall #-}

import Day

solution :: String -> String
solution = show . length . say 40

main :: IO ()
main = apply solution
