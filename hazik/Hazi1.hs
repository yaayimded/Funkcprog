module Hazi1 where

intExpr1, intExpr2, intExpr3 :: Int
intExpr1 = 1
intExpr2 = 2
intExpr3 = 3

charExpr1, charExpr2, charExpr3 :: Char
charExpr1 = 'a'
charExpr2 = 'b'
charExpr3 = 'c'

boolExpr1, boolExpr2, boolExpr3 :: Bool
boolExpr1 = True
boolExpr2 = False
boolExpr3 = 3 > 4

inc :: Integer -> Integer
inc = (+) 1

triple :: Integer -> Integer
triple = (*) 3

thirteen1 :: Integer  
thirteen1 = inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 0))))))))))))

thirteen2 :: Integer
thirteen2 = inc (inc (inc (inc (triple (triple (inc 0))))))

thirteen3 :: Integer
thirteen3 = inc (triple (inc (inc (inc (inc 0)))))

cmpRem5Rem7 :: Int -> Bool
cmpRem5Rem7 x = mod x 5 > mod x 7

foo :: Int -> Bool -> Bool
foo x y = (==) 5 x

bar :: Bool -> Int -> Bool
bar x y = foo y x

