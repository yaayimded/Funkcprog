module Hazi3 where

import Data.List(group)

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _ = False

exactly2OrAtLeast4 :: [a] -> Bool
exactly2OrAtLeast4 (x : y : []) = True
exactly2OrAtLeast4 (a : b : c : d : rs) = True
exactly2OrAtLeast4 _ = False 

firstTwoElements :: [a] -> [a]
firstTwoElements [] = []
firstTwoElements (a : []) = []
firstTwoElements (a : b : rs) = a : b : []

withoutThird :: [a] -> [a]
withoutThird (a : b : c : rs) = a : b : rs
withoutThird xs = xs 

onlySingletons :: [[a]] -> [[a]]
onlySingletons xs = [x | x <- xs, isSingleton x]

compress :: (Eq a, Num b) => [a] -> [(a,b)]
compress xs = [(head a, fromIntegral (length a)) | a <- group xs]

decompress :: Integral b => [(a,b)] -> [a]
decompress xs = [a | (a, b) <- xs, _ <- [1..b]]
