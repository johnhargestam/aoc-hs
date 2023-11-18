{-# OPTIONS_GHC -Wall #-}

import Day

isSubrangeOf :: Range -> Range -> Bool
isSubrangeOf (Range x1 y1) (Range x2 y2) = x1 >= x2 && y1 <= y2

fullyOverlaps :: Pair -> Bool
fullyOverlaps (x,y) =  x `isSubrangeOf` y || y `isSubrangeOf` x

solution :: String -> String
solution = show . length . filter fullyOverlaps . pairs . lines

main :: IO ()
main = apply solution
