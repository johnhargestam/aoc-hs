{-# OPTIONS_GHC -Wall #-}

module Utils.Parsec where

import Text.Parsec (parse, count, many1, string, many, try, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit, letter)
import Control.Monad (void)

parseF :: Parser a -> String -> a
parseF p s = case parse p "" s of
  Right val -> val
  Left  err -> errorWithoutStackTrace $ show err

sepByN :: Int -> Parser a -> Parser b -> Parser [a]
sepByN 0 _ _   = return []
sepByN n p sep = do
  x <- p
  xs <- count (n - 1) (sep >> p)
  return (x:xs)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (try (sep >> p))
  return (x:xs)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

digits :: Parser String
digits = many1 digit

letters :: Parser String
letters = many1 letter

space :: Parser ()
space = void (char ' ')

spaces :: Parser ()
spaces = void (many1 space)

newline :: Parser ()
newline = void (char '\n')

string :: String -> Parser ()
string s = void (Text.Parsec.string s)
