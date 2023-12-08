{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Utils.Parsec (parseF)
import Data.List (sortBy, partition)

cardStrength :: Char -> Int
cardStrength 'A' = 13
cardStrength 'K' = 12
cardStrength 'Q' = 11
cardStrength 'T' = 10
cardStrength 'J' = 1
cardStrength  n  = digitToInt n

cardStrengths :: Hand -> [Int]
cardStrengths = map cardStrength . cards

withHead :: (Maybe a -> a) -> [a] -> [a]
withHead f (x:xs) = f (Just x) : xs
withHead f []     = [f Nothing]

addOrSet :: Int -> Maybe Int -> Int
addOrSet x = maybe x (+x)

sortedKindsWithJoker :: [Char] -> [Int]
sortedKindsWithJoker cs = 
  let (js, xs) = partition (=='J') cs
  in withHead (addOrSet (length js)) $ sortedKinds xs

handStrength :: Hand -> Int
handStrength h = case sortedKindsWithJoker $ cards h of
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
