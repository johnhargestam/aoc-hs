{-# OPTIONS_GHC -Wall #-}

import Day
import Data.List (isInfixOf)

hasRepeatOneBetween :: Eq a => [a] -> Bool
hasRepeatOneBetween (a:b:c:ds) = a == c || hasRepeatOneBetween (b:c:ds)
hasRepeatOneBetween _ = False

hasPairTwice :: Eq a => [a] -> Bool
hasPairTwice (a:b:cs) = [a,b] `isInfixOf` cs || hasPairTwice (b:cs)
hasPairTwice _ = False

isNice :: String -> Bool
isNice s = hasRepeatOneBetween s && hasPairTwice s

solution :: String -> String
solution = show . length . filter isNice . lines

main :: IO ()
main = apply solution
