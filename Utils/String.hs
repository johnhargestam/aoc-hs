{-# OPTIONS_GHC -Wall #-}

module Utils.String where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace
