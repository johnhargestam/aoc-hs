{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (isDigit)
import Data.List (partition)

parts :: [(String, XY)] -> [Int]
parts = map number . uncurry neighbours . partition isSymbol
  where isSymbol = not . isDigit . head . fst
        neighbours = filter . flip (any . areNeighbours)
        number = read . fst

solution :: String -> String
solution = show . sum . parts . tokens

main :: IO ()
main = apply solution
