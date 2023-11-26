{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (sort)

requiredRibbon :: (Int, Int, Int) -> Int
requiredRibbon (l,w,h) = 2*a + 2*b + l*w*h
   where a:b:_ = sort [l, w, h]

solution :: String -> String
solution = show . sum . map (requiredRibbon . readDimension) . lines

main :: IO ()
main = apply solution
