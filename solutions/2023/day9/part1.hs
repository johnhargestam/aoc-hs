{-# OPTIONS_GHC -Wall #-}

import Day

predictNext :: [Int] -> Int
predictNext = sum . map last . takeUntil (all (== 0)) . iterate diffs

solution :: String -> String
solution = show . sum . map (predictNext . numbers) . lines

main :: IO ()
main = apply solution
