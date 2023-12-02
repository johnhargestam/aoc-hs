{-# OPTIONS_GHC -Wall #-}

import Day

match :: (Char, Char) -> Bool
match (x,y) = x == y

readT :: (Char, Char) -> Int
readT = read . return . fst

matches :: String -> [Int]
matches s = map readT $ filter match $ zip s $ drop (length s `div` 2) $ cycle s

solution :: String -> String
solution = show . sum . matches

main :: IO ()
main = apply solution
