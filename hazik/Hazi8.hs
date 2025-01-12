module Hazi8 where

import Data.Maybe

dropMaybes :: [Maybe a] -> [a]
dropMaybes [] = []
dropMaybes (Nothing : rs) = dropMaybes rs
dropMaybes ((Just x) : rs) = x : dropMaybes rs

data ListWithHoles a = Nil | Cons a (ListWithHoles a) | Hole (ListWithHoles a) deriving (Eq,Foldable)

instance Show a => Show (ListWithHoles a) where
  show :: ListWithHoles a -> String
  show xs = showBetter xs 0 where
    showBetter :: ListWithHoles a -> Int -> String
    showBetter xs 0 = "[" ++ showBetter xs 1
    showBetter Nil _ = "]"
    showBetter (Hole l) n 
      | n == 1 = "_" ++ showBetter l 2
      | otherwise = ",_" ++ showBetter l (n + 1)
    showBetter (Cons a l) n 
      | n == 1 = show a ++ showBetter l 2
      | otherwise = "," ++ show a ++ showBetter l (n + 1)
 
dehole :: ListWithHoles a -> [a]
dehole Nil = []
dehole (Hole a) = dehole a
dehole (Cons a rs) = a : dehole rs

fromMaybeList :: [Maybe a] -> ListWithHoles a
fromMaybeList [] = Nil
fromMaybeList (Nothing : rs) = Hole (fromMaybeList rs)
fromMaybeList (Just a : rs) = Cons a (fromMaybeList rs)

preserveHoles :: ListWithHoles a -> [Maybe a]
preserveHoles Nil = []
preserveHoles (Hole l) = Nothing : preserveHoles l
preserveHoles (Cons a l) = Just a : preserveHoles l

fillHoles :: ListWithHoles a -> a -> [a]
fillHoles Nil _ = []
fillHoles (Hole l) x = x : fillHoles l x
fillHoles (Cons a l) x = a : fillHoles l x

data NonEmptyList a = Last a | NECons a (NonEmptyList a) deriving (Eq, Show)
data Stream a = SCons a (Stream a) deriving (Show, Eq)

neLength :: Num b => NonEmptyList a -> b
neLength (Last _) = 1
neLength (NECons a xs) = 1 + neLength xs

eqUpTo :: (Eq a, Integral i) => i -> Stream a -> Stream a -> Bool
eqUpTo 0 _ _ = True
eqUpTo n undefined (SCons b bs) | n > 0 = False 
eqUpTo n (SCons b bs) undefined | n > 0 = False 
eqUpTo n (SCons a as) (SCons b bs) = a == b && eqUpTo (n - 1) as bs
