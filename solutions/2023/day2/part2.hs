{-# OPTIONS_GHC -Wall #-}

import Day

mostColors :: Colors -> Colors -> Colors
mostColors x y = Colors { red = most red, green = most green, blue = most blue }
  where most f = max (f x) (f y)

requiredColors :: [Colors] -> Colors
requiredColors = foldr1 mostColors

power :: Colors -> Int
power c = red c * green c * blue c

solution :: String -> String
solution = show . sum . map (power . requiredColors . colors . readGame) . lines

main :: IO ()
main = apply solution
