module Hazi4 where
import Data.List

mountain :: Integral i => i -> String
mountain 0 = ""
mountain n = mountain (n - 1) ++ genericReplicate (fromIntegral n) '#' ++ "\n"

countAChars :: Num i => String -> i
countAChars [] = 0
countAChars ('a' : xs) = 1 + countAChars xs
countAChars (_ : xs) = countAChars xs

lucas :: (Integral a, Num b) => a -> b
lucas 0 = 2
lucas 1 = 1
lucas n = lucas (n - 1) + lucas (n - 2)

toZero :: Integral i => i -> Bool -> i
toZero n True = 1
toZero n False = n

longerThan :: Integral i => [a] -> i -> Bool
longerThan [] 0 = False
longerThan [] n = n < 0
longerThan (x : []) n = n < 1  
longerThan (x : xs) 0 = True
longerThan (x : xs) n = longerThan xs (toZero n (n < 0) - 1)

format :: Integral i => i -> String -> String
format 0 str = str
format n [] = ' ' : format (n - 1) []
format n (x : xs) = x : format (n - 1) xs

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = x : y : merge xs ys
