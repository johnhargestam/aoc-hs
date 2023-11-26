{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (sort, group)

solution :: String -> String
solution = show . length . group . sort . positions

main :: IO ()
main = apply solution
