module Hazi7 where

data TriBool = Yes | Maybe | No deriving (Show, Eq) 

instance Ord TriBool where
  No <= _ = True
  Maybe <= Yes = True
  Maybe <= Maybe = True
  Maybe <= No = False
  Yes <= Yes = True
  Yes <= _ = False

triOr :: TriBool -> TriBool -> TriBool
triOr Yes _ = Yes
triOr _ Yes = Yes
triOr Maybe _ = Maybe
triOr _ Maybe = Maybe
triOr _ _ = No

triAnd :: TriBool -> TriBool -> TriBool
triAnd Yes Yes = Yes
triAnd No _ = No
triAnd _ No = No
triAnd Yes Maybe = Maybe
triAnd Maybe Yes = Maybe

incMonotonityTest :: (Integral i, Ord a) => i -> [a] -> TriBool
incMonotonityTest 0 [] = Yes
incMonotonityTest 0 (x : xs) = Maybe
incMonotonityTest 1 (x : y : xs) = Maybe 
incMonotonityTest n [xs] = Yes
incMonotonityTest n (x : y : rs) | x < y = incMonotonityTest (n - 1) (y : rs)
  | otherwise = No

data GolfScore = Ace | Albatross | Eagle | Birdie | Par | Bogey Integer deriving Show

instance Eq GolfScore where
  Ace == Ace = True
  Albatross == Albatross = True
  Eagle == Eagle = True
  Birdie == Birdie = True
  Par == Par = True
  Bogey n1 == Bogey n2 = n1 == n2
  _ == _ = False  

score :: Integer -> Integer -> GolfScore 
score x y | y == 1 = Ace
  | x - 3 >= y = Albatross
  | x - 2 >= y = Eagle
  | x - 1 >= y = Birdie
  | x == y = Par
  | otherwise = Bogey (y - x)

data Time = T Int Int deriving Eq 

t :: Int -> Int -> Time
t o p | 0 <= o && o <= 23 && 0 <= p && p <= 59 = T o p

instance Show Time where
  show (T o p) | p < 10 = show o ++ ":0" ++ show p
    | otherwise = show o ++ ":" ++ show p

instance Ord Time where
  T o1 p1 <= T o2 p2 = (o1 == o2 && p1 <= p2) || o1 < o2

isBetween :: Time -> Time -> Time -> Bool
isBetween (T o1 p1) (T o2 p2) (T o3 p3) = (o1 <= o2 && o2 <= o3 && p1 <= p2 && p2 <= p3) || (o1 >= o2 && o2 >= o3 && p1 >= p2 && p2 >= p3)

data USTime = AM Int Int | PM Int Int deriving Eq 

ustimeAM :: Int -> Int -> USTime
ustimeAM o p | 1 <= o && o <= 12 && 0 <= p && p <= 59 = AM o p
  | otherwise = error "AM hiba"

ustimePM :: Int -> Int -> USTime
ustimePM o p | 1 <= o && o <= 12 && 0 <= p && p <= 59 = PM o p
  | otherwise = error "PM hiba"

instance Show USTime where
  show (AM o p) | p < 10 = "AM " ++ show o ++ ":0" ++ show p
    | otherwise = "AM " ++ show o ++ ":" ++ show p
  show (PM o p) | p < 10 = "PM " ++ show o ++ ":0" ++ show p
    | otherwise = "PM " ++ show o ++ ":" ++ show p

instance Ord USTime where
  AM 12 p1 <= AM 12 p2 = p1 <= p2
  AM 12 _ <= AM _ _ = True
  AM _ _ <= AM 12 _ = False
  AM o1 p1 <= AM o2 p2 = o1 < o2 || (o1 == o2 && p1 <= p2)
  PM 12 p1 <= PM 12 p2 = p1 <= p2
  PM 12 _ <= PM _ _ = True
  PM _ _ <= PM 12 _ = False
  PM o1 p1 <= PM o2 p2 = o1 < o2 || (o1 == o2 && p1 <= p2)
  AM _ _ <= PM _ _ = True
  PM _ _ <= AM _ _ = False  

ustimeToTime :: USTime -> Time
ustimeToTime (AM o p) | o == 12 = T 0 p
  | otherwise = T o p
ustimeToTime (PM o p) | o == 12 = T o p
  | otherwise = T (o + 12) p

timeToUSTime :: Time -> USTime
timeToUSTime (T o p) | o == 0 = AM 12 p
  | o == 12 = PM 12 p
  | 12 < o = PM (o - 12) p
  | otherwise = AM o p
