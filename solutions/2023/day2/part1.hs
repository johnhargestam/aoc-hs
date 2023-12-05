{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF)

isPossible :: Game -> Bool
isPossible = not . any (\c -> red c > 12 || green c > 13 || blue c > 14) . rounds

solution :: String -> String
solution = show . sum . map gameId . filter isPossible . map (parseF gameP) . lines

main :: IO ()
main = apply solution
