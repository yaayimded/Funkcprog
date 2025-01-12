module RopZH08 where

data EitherOrNothing a b = Left' a | Right' b | Nothing' deriving (Eq, Show)

