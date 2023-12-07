{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Char (digitToInt)
import Data.List (group, sort, sortBy)
import Utils.List (descending)
import Text.Parsec.String (Parser)
import Text.Parsec (count, oneOf)
import Utils.Parsec (parseF, space, digits, newline, sepBy1)
import Data.Ord (comparing)
import Distribution.Simple.Utils (equating)

data Hand = Hand { cards :: [Char], bid :: Int }
  deriving Show

handP :: Parser Hand
handP = do
  cs <- count 5 (oneOf "AKQJT98765432")
  space
  b <- read <$> digits
  return (Hand cs b)

handsP :: Parser [Hand]
handsP = sepBy1 handP newline

instance Eq Hand where
  (==) = equating cardStrengths

instance Ord Hand where
  compare = comparing handStrength <> comparing cardStrengths  

cardStrength :: Char -> Int
cardStrength 'A' = 14
cardStrength 'K' = 13
cardStrength 'Q' = 12
cardStrength 'J' = 11
cardStrength 'T' = 10
cardStrength  n  = digitToInt n

cardStrengths :: Hand -> [Int]
cardStrengths = map cardStrength . cards

kinds :: Ord a => [a] -> [Int]
kinds = sortBy descending . map length . group . sort

handStrength :: Hand -> Int
handStrength (Hand cs _) = case kinds cs of
 [5]       -> 6
 [4,1]     -> 5
 [3,2]     -> 4
 [3,1,1]   -> 3
 [2,2,1]   -> 2
 [2,1,1,1] -> 1
 _         -> 0

winnings :: [Hand] -> [Int]
winnings = zipWith (*) [1..] . map bid . sort

solution :: String -> String
solution = show . sum . winnings . parseF handsP

main :: IO ()
main = apply solution
