{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF, space, digits, sepBy1, sepByN)
import Text.Parsec.String (Parser)

data Range = Range
 { start :: Int, size :: Int }
  deriving Show

rangeP :: Parser Range
rangeP = do
  [a,b] <- map read <$> sepByN 2 digits space
  return (Range a b)

rangesP :: Parser [Range]
rangesP = sepBy1 rangeP space

solution :: String -> String
solution = show . parseF (almanacP rangesP)

main :: IO ()
main = apply solution
