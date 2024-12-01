{-# OPTIONS_GHC -Wall #-}

import Data.List (sort)
import Day
import Text.Parsec.String (Parser)
import Utils.Parsec (digits, parseF, spaces)

locationsP :: Parser (Int, Int)
locationsP = do
        n <- digits
        spaces
        m <- digits
        return (read n, read m)

parseLines :: String -> [(Int, Int)]
parseLines = map (parseF locationsP) . lines

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

diff :: (Num a) => (a, a) -> a
diff = abs . uncurry (-)

solution :: String -> String
solution = show . sum . map diff . uncurry zip . mapBoth sort . unzip . parseLines

main :: IO ()
main = apply solution
