{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (sieve, splitEq)
import Utils.Grid (listToGrid, Grid, walking, south, east, value)
import Data.Char (isAsciiUpper)
import Data.Sequence (fromList, Seq, adjust, index)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Foldable (toList)
import Text.Printf (printf)
import Data.List (iterate')

data Move = Move { stack :: Int, from :: Int, to :: Int } deriving Show
type Stacks = Seq String

-- Parsing

{-["    [D]    ",    [" D ",
   "[N] [C]    ", ->  "NC ",
   "[Z] [M] [P]"]     "ZMP"] -}
isolateCrates :: [String] -> [String]
isolateCrates = map (sieve 4 . drop 1)

topRows :: Grid a -> [Grid a]
topRows = walking east

crates :: Grid Char -> String
crates = filter isAsciiUpper . map value . walking south

stacks :: [String] -> Seq String
stacks = fromList . map crates . topRows . listToGrid . isolateCrates

readMove :: String -> Maybe Move
readMove = readMove' . mapMaybe readMaybe . words
  where readMove' [n,f,t] = Just $ Move n f t
        readMove' _       = Nothing

moves :: [String] -> [Move]
moves = mapMaybe readMove

parse :: String -> (Stacks, [Move])
parse s = parse' (splitEq "\n\n" s)
  where
    parse' [x,y] = (stacks (init (lines x)), moves (take 5 (lines y)))
    parse' _     = error "invalid input data"

-- Solving

move :: Int -> Int -> Stacks -> Stacks
move fr to s = move' (index s fr)
  where move' (v:_) = adjust (v:) to $ adjust tail fr s
        move' _     = error $ printf "illegal move from %s to %s" (show (fr+1)) (show (to+1))

moveN :: Move -> Stacks -> Stacks
moveN (Move n fr to) = last . take n . iterate' (move (fr-1) (to-1))

moveN2 :: Move -> Stacks -> Stacks
moveN2 (Move n fr to) s = iterate (move (fr-1) (to-1)) s !! n

rearrange :: Stacks -> [Move] -> Stacks
rearrange = foldr moveN2

topCrates :: Stacks -> [Char]
topCrates = map head . toList

solution :: String -> String
solution = show . uncurry rearrange . parse

main :: IO ()
main = apply solution
