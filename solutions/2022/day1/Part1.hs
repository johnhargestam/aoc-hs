{-# OPTIONS_GHC -Wall #-}

import Day (solve, calorieSums)

solution :: String -> String
solution = show . maximum . calorieSums

main :: IO ()
main = solve solution
