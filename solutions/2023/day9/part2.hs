{-# OPTIONS_GHC -Wall #-}

import Day

predictPrevious :: [Int] -> Int
predictPrevious = foldr1 (-) . map head . takeUntil (all (== 0)) . iterate diffs

solution :: String -> String
solution = show . sum . map (predictPrevious . numbers) . lines

main :: IO ()
main = apply solution
