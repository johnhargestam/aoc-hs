{-# OPTIONS_GHC -Wall #-}

import Day

twoAdjacent :: Eq a => [a] -> Bool
twoAdjacent (x:xs) = duplicates == 1 || twoAdjacent (drop duplicates xs)
  where duplicates = length $ takeWhile (==x) xs
twoAdjacent _      = False

meetsCriteria :: [[Int]] -> [[Int]]
meetsCriteria = filter twoAdjacent . filter neverDecrease

solution :: String -> String
solution = show . length . meetsCriteria . map digits . parseRange

main :: IO ()
main = apply solution
