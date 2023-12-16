{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF)
import Data.Bifunctor (second)
import Utils.List (interleave)

incrementHead :: Num a => [a] -> [a]
incrementHead (x:xs) = x+1 : xs
incrementHead xs     = xs

incrementAt :: Num a => Int -> [a] -> [a]
incrementAt i = uncurry (++) . second incrementHead . splitAt i

distributions :: (Num a, Eq a) => Int -> Int -> [[a]]
distributions 0   len = [replicate len 0]
distributions tot len = concatMap (go tot (replicate len 0)) [0..len-1]
  where go 1 ys i = [incrementAt i ys]
        go n ys i = concatMap (go (n-1) (incrementAt i ys)) [i..len-1]

areSeperated :: [Int] -> Bool
areSeperated (_:0:_:_) = False
areSeperated (_:xs)    = areSeperated xs
areSeperated []        = True

totalSpace :: Springs -> Int
totalSpace (Springs row groups) = length row - sum groups

spaces :: Springs -> Int
spaces (Springs _ groups) = length groups + 1

spacings :: Springs -> [[Int]]
spacings s = filter areSeperated $ distributions (totalSpace s) (spaces s)

toRow :: [String] -> [String] -> String
toRow xs ys = concat $ interleave ys xs

draw :: [Int] -> [[Int]] -> [String]
draw gs = map (toRow (map (`replicate` '#') gs) . map (`replicate` '.'))

arrangements :: Springs -> [String]
arrangements s = draw (groups s) $ spacings s

charMatch :: Char -> Char -> Bool
charMatch '?' _ = True
charMatch _ '?' = True
charMatch x y   = x == y

rowMatch :: String -> String -> Bool
rowMatch template = and . zipWith charMatch template

legalArrangements :: Springs -> [String]
legalArrangements s = filter (rowMatch (row s)) $ arrangements s

solution :: String -> String
solution = show . sum . map (length . legalArrangements . parseF springsRowP). lines

main :: IO ()
main = apply solution
