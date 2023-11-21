{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (splitEq)
import Data.List (intersect)

data Way = U Int
         | D Int
         | L Int
         | R Int
         deriving Show

type Point = (Int, Int)

type Line = (Point, Point)

move :: Point -> Way -> Point
move (x,y) (U n) = (x, y+n)
move (x,y) (D n) = (x, y-n)
move (x,y) (L n) = (x-n, y)
move (x,y) (R n) = (x+n, y)

turns :: [Way] -> [Point]
turns = scanl move (0,0)

connect:: [Point] -> [Line]
connect pts = zip pts (drop 1 pts)

bridge :: Line -> [Point]
bridge ((x1, y1), (x2, y2))
    | x1 == x2  = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2  = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = undefined

intersection :: (Line, Line) -> [Point]
intersection (l1, l2) = bridge l1 `intersect` bridge l2

crossings :: [[Line]] -> [Point]
crossings [w1, w2] = concatMap intersection pairs
  where pairs = [(x, y) | x <- w1, y <- w2]
crossings _        = undefined

distance :: Point -> Int
distance (x, y) = abs x + abs y

closest :: [Point] -> Int
closest = minimum . filter (>0) . map distance

readWay :: String -> Way
readWay ('U':n) = U (read n)
readWay ('D':n) = D (read n)
readWay ('L':n) = L (read n)
readWay ('R':n) = R (read n)
readWay _ = undefined

wires :: [String] -> [[Line]]
wires = map (connect . turns . parseLine)

parseLine :: String -> [Way]
parseLine = map readWay . splitEq ","

solution :: String -> String
solution = show . closest . crossings . wires . lines

main :: IO ()
main = apply solution
