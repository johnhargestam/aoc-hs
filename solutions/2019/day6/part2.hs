{-# OPTIONS_GHC -Wall #-}

import Day
import Data.Tree (Tree (Node))
import Utils.List (findWithDefault, lastMaybe)
import Data.List ((\\))

pathToNode :: Eq a => a -> Tree a -> [a]
pathToNode v (Node x _) | v == x = [x]
pathToNode _ (Node _ [])         = []
pathToNode v (Node x xs)         = x : findWithDefault [] hasValue subPaths
  where subPaths = map (pathToNode v) xs
        hasValue = (Just v ==) . lastMaybe

nodeDistance :: Eq a => a -> a -> Tree a -> Int
nodeDistance x y t = length ((xPath \\ yPath) ++ (yPath \\ xPath)) - 2
  where xPath = pathToNode x t
        yPath = pathToNode y t

solution :: String -> String
solution = show . nodeDistance "YOU" "SAN". readTree

main :: IO ()
main = apply solution
