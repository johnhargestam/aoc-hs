{-# OPTIONS_GHC -Wall #-}

module Utils.Parsec where

import Text.Parsec (Parsec, parse)

parseF :: Parsec String () a -> String -> a
parseF p s = case parse p "" s of
  Right val -> val
  Left  err -> error $ show err
