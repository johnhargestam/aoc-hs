{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF)

mostColors :: Round -> Round -> Round
mostColors x y = Round { red = most red, green = most green, blue = most blue }
  where most f = max (f x) (f y)

requiredColors :: [Round] -> Round
requiredColors = foldr1 mostColors

power :: Round -> Int
power c = red c * green c * blue c

solution :: String -> String
solution = show . sum . map (power . requiredColors . rounds . parseF gameP) . lines

main :: IO ()
main = apply solution
