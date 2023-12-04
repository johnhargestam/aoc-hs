{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parser (parse)

isPossible :: Game -> Bool
isPossible = not . any (\c -> red c > 12 || green c > 13 || blue c > 14) . rounds

solution :: String -> String
solution = show . sum . map gameId . filter isPossible . map (parse gameP) . lines

main :: IO ()
main = apply solution
