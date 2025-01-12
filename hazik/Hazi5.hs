module Hazi5 where

import Data.Char(toUpper, isDigit, isSpace)
import Data.List(intersperse, words)

toUpperThird :: String -> String
toUpperThird [] = []
toUpperThird (a : []) = (a : [])
toUpperThird (a : b : []) = (a : b : [])
toUpperThird (a : b : c : rs) = (a : b : toUpper c : rs)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : y : rs) = x <= y && isSorted (y : rs)

(!!!) :: Integral b => [a] -> b -> a
(!!!) (x : xs) 0 = x
(!!!) xs n | n < 0 = reverse xs !!! abs (n + 1)
(!!!) (x : xs) n = (!!!) xs (n - 1)

format :: Integral i => i -> String -> String
format n str | n <= 0 = str
format n [] = ' ' : format (n - 1) []
format n (x : xs) = x : format (n - 1) xs

mightyGale :: (Num a, Ord b, Num b, Integral c) => [(String, a, b, c)] -> String
mightyGale [] = ""
mightyGale ((nev, _, v, _) : rs) | v > 110 = nev 
  | otherwise = mightyGale rs 

cipher :: String -> String
cipher [] = ""
cipher (x : []) = ""
cipher (x : y : []) = ""
cipher (x : y : z : rs) | isDigit z = (x : y : [])
  | otherwise = cipher (y : z : rs)

doubleElements :: [a] -> [a]
doubleElements [] = []
doubleElements (x : xs) = x : x : doubleElements xs

deleteDuplicateSpaces :: String -> String
deleteDuplicateSpaces xs = concat (intersperse " " (words xs))  

