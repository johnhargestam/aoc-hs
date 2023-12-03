{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (isDigit)
import Utils.List (mapWithIndex)
import Data.List (groupBy, partition)
import Data.Bifunctor (Bifunctor(first))

type XY = (Int, Int)

-- ("33",(5,5)) ('4',(7,5)) -> ("334",(5,5))
joinChars :: (String, XY) -> (Char, XY) -> (String, XY)
joinChars (s, xy) (c, _) = (s ++ [c], xy)

-- 0 "467..114..*" -> [("467",(0,0)), ("114",(5,0)), ("*",(10,0))]
line :: Int -> String -> [(String, XY)]
line y = map toString . filter notDot . groupBy isNumber . mapWithIndex charXY
  where charXY x c = (c, (x, y))
        isNumber (a, _) (b, _) = isDigit a && isDigit b
        notDot = (/='.') . fst . head
        toString cs = foldl joinChars (first (: []) $ head cs) $ tail cs

tokens :: String -> [(String, XY)]
tokens = concat . mapWithIndex line . lines

areNeighbours :: (String, XY) -> (String, XY) -> Bool
areNeighbours (a, (x1, y1)) (b, (x2, y2))
  | y1 == y2         = x2 - x1 == length a || x1 - x2 == length b
  | abs (y2-y1) == 1 = x1 + length a >= x2 && x2 + length b >= x1
  | otherwise        = False

parts :: [(String, XY)] -> [Int]
parts = map number . uncurry neighbours . partition isSymbol
  where isSymbol = not . isDigit . head . fst
        neighbours = filter . flip (any . areNeighbours)
        number = read . fst

solution :: String -> String
solution = show . sum . parts . tokens

main :: IO ()
main = apply solution
