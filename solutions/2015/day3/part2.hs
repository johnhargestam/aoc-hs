{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (sort, group)

uninterleave :: [a] -> ([a], [a])
uninterleave = foldr everyOther ([],[])
  where everyOther x (xs,ys) = (x:ys, xs)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x,y) = (f x, f y)

concatPair :: ([a], [a]) -> [a]
concatPair (xs,ys) = xs ++ ys

positions2 :: [Char] -> [(Int, Int)]
positions2 = concatPair . mapBoth positions . uninterleave

solution :: String -> String
solution = show . length . group . sort . positions2

main :: IO ()
main = apply solution
