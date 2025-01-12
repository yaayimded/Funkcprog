module Hazi2 where

addV :: (Double, Double) -> (Double, Double) -> (Double, Double)
addV (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

subV :: (Double, Double) -> (Double, Double) -> (Double, Double)
subV (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

scaleV :: Double -> (Double, Double) -> (Double, Double)
scaleV s (x, y) = (x * s, y * s)

scalar :: (Double, Double) -> (Double, Double) -> Double
scalar (x0, y0) (x1, y1) = x0 * x1 + y0 * y1

divides :: Integral a => a -> a -> Bool
divides 0 0 = True
divides 0 _ = False
divides x y = mod y x == 0

add :: (Integral a, Integral b, Num c) => a -> b -> c
add x y = fromIntegral x + fromIntegral y 
