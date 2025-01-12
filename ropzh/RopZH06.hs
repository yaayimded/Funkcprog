module RopZH06 where

type Person = (String, String, Int)

isTeenager :: Person -> Bool
isTeenager (_, _, n) | n >= 10 && n <= 19 = True
  | otherwise = False

