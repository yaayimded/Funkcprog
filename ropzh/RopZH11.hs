module RopZH11 where

import Data.List(genericLength)

countOnes :: (Show a, Num b) => a -> b
countOnes = genericLength . filter (=='1') . show
