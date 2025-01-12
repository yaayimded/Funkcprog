module RopZH02 where

addVec3, subVec3 :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
addVec3 (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

subVec3 (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

rotateVec2 :: Num a => (a, a) -> (a, a)
rotateVec2 (x, y) = (y, -x)
