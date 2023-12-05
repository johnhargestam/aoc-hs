{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)

inMap :: Int -> Map -> Maybe Int
inMap x m | y <- x - source m, y >= 0 && y < size m = Just $ destination m + y
          | otherwise = Nothing

convert :: Int -> Converter -> Int
convert x c = fromMaybe x . listToMaybe $ mapMaybe (x `inMap`) $ maps c

location :: [Converter] -> Int -> Int
location = flip $ foldl convert

locations :: Almanac -> [Int]
locations (Almanac seeds cs) = map (location cs) seeds

solution :: String -> String
solution = show . minimum . locations . parseInput

main :: IO ()
main = apply solution
