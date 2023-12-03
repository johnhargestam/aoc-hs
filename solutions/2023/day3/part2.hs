{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (isDigit)
import Data.List (partition)

toRatio :: [(String, b)] -> Int
toRatio = product . map (read . fst) 

ratios :: [(String, XY)] -> [Int]
ratios = map toRatio . filter exactlyTwo . uncurry neighbours . partition isNumber
  where isNumber = isDigit . head . fst
        neighbours = map . flip (filter . areNeighbours)
        exactlyTwo = (==2) . length

solution :: String -> String
solution = show . sum . ratios . tokens

main :: IO ()
main = apply solution
