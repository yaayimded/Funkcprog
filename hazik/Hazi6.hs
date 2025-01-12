module Hazi6 where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn n (x : xs) | x == n = [] : splitOn n xs
  | otherwise = (x : head (splitOn n xs)) : tail (splitOn n xs) 

emptyLines :: Num a => String -> [a]
emptyLines [] = [1]
emptyLines (x : y : rs) | x == '\n' && y == '\n' = 

csv :: String -> [[String]]
csv [] = [[]]
csv str = sor (newLine' str) where
  sor :: [String] -> [[String]]
  sor [] = []
  sor (x : xs) = splitOn ',' x : sor xs

  newLine' :: String -> [String]
  newLine' [] = [[]]
  newLine' (x : xs) | x == '\n' = [] : newLine' xs
    | otherwise = [x] : newLine' xs
