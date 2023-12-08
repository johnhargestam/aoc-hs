{-# OPTIONS_GHC -Wall #-}

import Day
import Utils.Parsec (parseF)

solution :: String -> String
solution = show . length . takeWhile (/= "ZZZ") . pathFrom "AAA" . parseF inputP

main :: IO ()
main = apply solution
