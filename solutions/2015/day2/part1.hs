{-# OPTIONS_GHC -Wall #-}

import Day

solution :: String -> String
solution = show . sum . map (requiredPaper . readDimension) . lines

main :: IO ()
main = apply solution
