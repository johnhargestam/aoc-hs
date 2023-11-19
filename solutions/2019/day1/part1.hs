{-# OPTIONS_GHC -Wall #-}

import Day

solution :: String -> String
solution = show . sum . map (fuel . read) . lines

main :: IO ()
main = apply solution
