{-# OPTIONS_GHC -Wall #-}

import Day

isPossible :: Game -> Bool
isPossible = not . any (\c -> red c > 12 || green c > 13 || blue c > 14) . colors

solution :: String -> String
solution = show . sum . map gameId . filter isPossible . map readGame . lines

main :: IO ()
main = apply solution
