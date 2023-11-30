{-# OPTIONS_GHC -Wall #-}

import Day

solution :: String -> String
solution = show . last . take 2 . filter isValid . nextPasswords

main :: IO ()
main = apply solution
