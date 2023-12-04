{-# OPTIONS_GHC -Wall #-}

module Utils.Parser (
  module Utils.Parser,
  module Text.ParserCombinators.ReadP
) where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isSpace, isLetter)

parse :: ReadP a -> String -> a
parse x s | [(y, "")] <- readP_to_S x s = y
parse _ _ = undefined

parseS :: ReadP a -> ReadS a
parseS = readP_to_S

digit :: ReadP Char
digit = satisfy isDigit

letter :: ReadP Char
letter = satisfy isLetter

space :: ReadP Char
space = satisfy isSpace
