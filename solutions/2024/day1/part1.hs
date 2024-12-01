{-# OPTIONS_GHC -Wall #-}

import Day
import Text.Parsec.String (Parser)
import Utils.Parsec (digits, parseF, spaces)

locationsP :: Parser (Int, Int)
locationsP = do
        n <- digits
        spaces
        m <- digits
        return (read n, read m)

solution :: String -> String
solution = show . map (parseF locationsP) . lines

main :: IO ()
main = apply solution
