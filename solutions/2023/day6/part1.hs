{-# OPTIONS_GHC -Wall #-}

import Day
import Text.Parsec.String (Parser)
import Text.Parsec (sepBy, eof)
import Utils.Parsec (digits, spaces, newline, string, parseF)

racesP :: Parser [Race]
racesP = do
  string "Time:"
  spaces
  ts <- map read <$> digits `sepBy` spaces
  newline
  string "Distance:"
  spaces
  rs <- map read <$> digits `sepBy` spaces
  eof
  return (zipWith Race ts rs)

solution :: String -> String
solution = show . product . map wins . parseF racesP

main :: IO ()
main = apply solution
