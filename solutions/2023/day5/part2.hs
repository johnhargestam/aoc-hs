{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF)

solution :: String -> String
solution = show . parseF almanacP

main :: IO ()
main = apply solution
