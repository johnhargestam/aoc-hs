{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Utils.Parsec (parseF)
import Data.List (sortBy)

cardStrength :: Char -> Int
cardStrength 'A' = 14
cardStrength 'K' = 13
cardStrength 'Q' = 12
cardStrength 'J' = 11
cardStrength 'T' = 10
cardStrength  n  = digitToInt n

cardStrengths :: Hand -> [Int]
cardStrengths = map cardStrength . cards

handStrength :: Hand -> Int
handStrength h = case sortedKinds $ cards h of
 [5]       -> 6
 [4,1]     -> 5
 [3,2]     -> 4
 [3,1,1]   -> 3
 [2,2,1]   -> 2
 [2,1,1,1] -> 1
 _         -> 0

rank :: [Hand] -> [Hand]
rank = sortBy (comparing handStrength <> comparing cardStrengths)

solution :: String -> String
solution = show . sum . winnings . rank . parseF handsP

main :: IO ()
main = apply solution
