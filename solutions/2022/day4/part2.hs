{-# OPTIONS_GHC -Wall #-}

import Day

intersects :: Range -> Range -> Bool
intersects (Range _ y1) (Range x2 _) = y1 >= x2

overlaps :: Pair -> Bool
overlaps (x,y)
       | x < y     = x `intersects` y
       | otherwise = y `intersects` x

solution :: String -> String
solution = show . length . filter overlaps . pairs . lines

main :: IO ()
main = apply solution
