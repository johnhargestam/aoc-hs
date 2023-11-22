{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (mapAdjacent)

someAdjacent :: Eq a => [a] -> Bool
someAdjacent = or . mapAdjacent (==)

meetsCriteria :: [[Int]] -> [[Int]]
meetsCriteria = filter someAdjacent . filter neverDecrease

solution :: String -> String
solution = show . length . meetsCriteria . map digits . parseRange

main :: IO ()
main = apply solution
