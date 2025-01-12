module RopZH09 where

multiplyPairs :: Num a => [a] -> [a] -> [a]
multiplyPairs [] _ = []
multiplyPairs _ [] = []
multiplyPairs xs ys = zipWith (\x y -> x * y) xs ys 

