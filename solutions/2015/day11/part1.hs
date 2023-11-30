{-# OPTIONS_GHC -Wall #-}

import Day

solution :: String -> String
solution = show . take 1 . filter isValid . nextPasswords

main :: IO ()
main = apply solution
