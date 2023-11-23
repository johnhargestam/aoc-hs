{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.List (splitEq)
import Data.Map (Map, fromListWith, findWithDefault)
import Data.Tree (Tree (Node), unfoldTree)

readKeyValue :: String -> (String, [String])
readKeyValue = tupled . splitEq ")"
  where tupled [x,y] = (x, [y])
        tupled _     = undefined

toKeyValues :: [String] -> [(String, [String])]
toKeyValues = map readKeyValue

toMap :: [(String, [String])] -> Map String [String]
toMap = fromListWith (++)

toTree :: Map String [String] -> Tree String
toTree m = unfoldTree build "COM"
  where build k = (k, findWithDefault [] k m)

sumDepths :: Tree String -> Int
sumDepths = go 0
  where go d (Node _ []) = d
        go d (Node _ xs) = d + sum (map (go (d+1)) xs)

solution :: String -> String
solution = show . sumDepths . toTree . toMap . toKeyValues . lines

main :: IO ()
main = apply solution
