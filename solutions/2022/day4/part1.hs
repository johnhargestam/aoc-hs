{-# OPTIONS_GHC -Wall #-}

import Day
import Utils (splitEq)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

data Range = Range Int Int deriving (Show)

type Pair = (Range, Range)

readRange :: String -> Maybe Range
readRange = readRange' . mapMaybe readMaybe . splitEq "-"
  where readRange' [x,y] = Just (Range x y)
        readRange' _     = Nothing

readPair :: String -> Maybe Pair
readPair = readPair' . mapMaybe readRange . splitEq ","
  where readPair' [x,y] = Just (x, y)
        readPair' _     = Nothing

pairs :: [String] -> [Pair]
pairs = mapMaybe readPair

isSubrangeOf :: Range -> Range -> Bool
isSubrangeOf (Range x1 y1) (Range x2 y2) = x1 >= x2 && y1 <= y2

fullyOverlaps :: Pair -> Bool
fullyOverlaps (x,y) =  x `isSubrangeOf` y || y `isSubrangeOf` x

solution :: String -> String
solution = show . length . filter fullyOverlaps . pairs . lines

main :: IO ()
main = apply solution
