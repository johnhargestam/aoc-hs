{-# OPTIONS_GHC -Wall #-}

import Data.List (sort)
import Day

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

diff :: (Num a) => (a, a) -> a
diff = abs . uncurry (-)

solution :: String -> String
solution = show . sum . map diff . uncurry zip . mapBoth sort . unzip . parseLines

main :: IO ()
main = apply solution
