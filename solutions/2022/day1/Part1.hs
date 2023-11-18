{-# OPTIONS_GHC -Wall #-}

import Day

solution :: String -> String
solution = show . maximum . calorieSums

main :: IO ()
main = apply solution
