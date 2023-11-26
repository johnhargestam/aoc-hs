{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (splitEq)

tuple3 :: [a] -> (a, a, a)
tuple3 [a,b,c] = (a, b, c)
tuple3 _       = undefined

readDimension :: String -> (Int, Int, Int)
readDimension = tuple3 . map read . splitEq "x"

requiredPaper :: (Int, Int, Int) -> Int
requiredPaper (l,w,h) = 2*lw + 2*wh + 2*hl + minimum [lw, wh, hl]
   where (lw, wh, hl) = (l*w, w*h, h*l)

solution :: String -> String
solution = show . sum . map (requiredPaper . readDimension) . lines

main :: IO ()
main = apply solution
