module RopZH3 where

cubeNumbersLastDigits :: Integral a => [a]
cubeNumbersLastDigits = [mod (x * x * x) 10 | x <- [1..]] 

dropFirstAndThird :: [a] -> [a]
dropFirstAndThird (a : b : c : rs) = b : rs
dropFirstAndThird xs = xs
