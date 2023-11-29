{-# OPTIONS_GHC -Wall #-}

import Day

solution :: String -> String
solution = show . length . say 50

main :: IO ()
main = apply solution
