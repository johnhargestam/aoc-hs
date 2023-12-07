{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Utils.Parsec (parseF, space, digits, sepBy1)
import Text.Parsec.String (Parser)

mapInt :: Int -> Map -> Maybe Int
mapInt x m | diff <- x - source m, diff >= 0 && diff < len m = Just $ destination m + diff
           | otherwise = Nothing

convertInt :: Int -> Converter -> Int
convertInt x c = fromMaybe x . listToMaybe $ mapMaybe (mapInt x) $ maps c

location :: [Converter] -> Int -> Int
location = flip $ foldl convertInt

locations :: Almanac Int -> [Int]
locations (Almanac seeds cs) = map (location cs) seeds

seedsP :: Parser [Int]
seedsP = map read <$> sepBy1 digits space

solution :: String -> String
solution = show . minimum . locations . parseF (almanacP seedsP)

main :: IO ()
main = apply solution
