{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (splitEq, mapAdjacent)
import Data.Char (digitToInt)


digits :: Int -> [Int]
digits = map digitToInt . show

parseRange :: String -> [Int]
parseRange = between . map read . splitEq "-"
  where between [x,y] = [x..y]
        between _     = undefined

hasSameAdjacent :: Eq a => [a] -> Bool
hasSameAdjacent = or . mapAdjacent (==)

neverDecrease :: Ord a => [a] -> Bool
neverDecrease = and . mapAdjacent (<=)

meetsCriteria :: [[Int]] -> [[Int]]
meetsCriteria = filter hasSameAdjacent . filter neverDecrease

solution :: String -> String
solution = show . length . meetsCriteria . map digits . parseRange

main :: IO ()
main = apply solution
