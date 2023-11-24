{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (mapWithIndex)
import Data.Maybe (catMaybes)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set

readLocation :: Int -> Int -> Char -> Maybe (Int, Int)
readLocation y x '#' = Just (x, y)
readLocation _ _ _   = Nothing

readLocations :: Int -> String -> [(Int, Int)]
readLocations y = catMaybes . mapWithIndex (readLocation y)

locations :: [String] -> [(Int, Int)]
locations =  concat . mapWithIndex readLocations

intermediates :: (Int, Int) -> [(Int, Int)]
intermediates (0, 0) = [(0,0)]
intermediates (x, y) = reverse . take n $ iterate step (x, y)
  where n = gcd x y
        (dx, dy) = (x `div` n, y `div` n)
        step (a, b) = (a-dx, b-dy)

firstMember :: Ord a => Set a -> [a] -> a
firstMember xs = head . filter (`Set.member` xs)

relativeTo :: (Int, Int) -> (Int, Int) -> (Int, Int)
relativeTo (x1, y1) (x2, y2) = (x2-x1, y2-y1)

withOrigo :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
withOrigo xs x = filter (/=(0,0)) $ map (relativeTo x) xs

inView :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
inView xs = Set.toList . inView' . Set.fromList . withOrigo xs
  where inView' set = Set.foldr (insertFirstIn set) Set.empty set
        insertFirstIn set = Set.insert . firstMember set . intermediates

bestView :: [(Int, Int)] -> [(Int, Int)]
bestView xs = maximumBy (comparing length) $ map (inView xs) xs

solution :: String -> String
solution = show . length . bestView . locations . lines

main :: IO ()
main = apply solution
