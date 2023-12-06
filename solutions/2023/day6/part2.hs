{-# OPTIONS_GHC -Wall #-}

import Day
import Text.Parsec.String (Parser)
import Text.Parsec (sepBy, eof)
import Utils.Parsec (digits, spaces, newline, string, parseF)

raceP :: Parser Race
raceP = do
  string "Time:"
  spaces
  t <- read . concat <$> digits `sepBy` spaces
  newline
  string "Distance:"
  spaces
  r <- read . concat <$> digits `sepBy` spaces
  eof
  return (Race t r)

solution :: String -> String
solution = show . wins . parseF raceP

main :: IO ()
main = apply solution
