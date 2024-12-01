{-# OPTIONS_GHC -Wall #-}

import Day

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

scores :: ([Int], [Int]) -> [Int]
scores (xs, ys) = map f xs
  where
    f x = x * count (x ==) ys

solution :: String -> String
solution = show . sum . scores . unzip . parseLines

main :: IO ()
main = apply solution
