{-# OPTIONS_GHC -Wall #-}

module Utils.Bool where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p ||| q = \v -> p v || q v

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p &&& q = \v -> p v && q v
