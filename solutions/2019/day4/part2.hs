{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (group)

twoAdjacent :: Eq a => [a] -> Bool
twoAdjacent = elem 2 . map length . group

meetsCriteria :: [[Int]] -> [[Int]]
meetsCriteria = filter twoAdjacent . filter neverDecrease

solution :: String -> String
solution = show . length . meetsCriteria . map digits . parseRange

main :: IO ()
main = apply solution
