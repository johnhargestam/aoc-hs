{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF)
import Data.Map ( keys)
import Data.List (transpose, isSuffixOf)
import Utils.List (countWhile)

pathsWhere :: (String -> Bool) -> ([Direction], NodeMap) -> [[String]]
pathsWhere p (ds, m) = transpose . map (`pathFrom` (ds, m)) $ filter p $ keys m

isStart :: String -> Bool
isStart = ("A" `isSuffixOf`)

isFinish :: String -> Bool 
isFinish = ("Z" `isSuffixOf`)

countUntil :: (a -> Bool) -> [a] -> Int
countUntil p = countWhile (not . p)

solution :: String -> String
solution = show . countUntil (all isFinish) . pathsWhere isStart . parseF inputP

main :: IO ()
main = apply solution
