module NagyBead where

import Data.Either
import Data.Maybe
import Text.Read(readMaybe)
import Data.List(genericLength)

basicInstances = 0 -- Mágikus tesztelőnek kell ez, NE TÖRÖLD!

data Dir = InfixL | InfixR deriving (Show, Eq, Ord)

data Tok a = BrckOpen | BrckClose | TokLit a | TokBinOp (a -> a -> a) Char Int Dir

instance Show a => Show (Tok a) where
  show BrckOpen = "BrckOpen"
  show BrckClose = "BrckClose"
  show (TokLit a) = "TokLit " ++ show a
  show (TokBinOp a b c d) = "TokBinOp " ++ show b ++ " " ++ show c ++ " " ++ show d

instance Eq a => Eq (Tok a) where
  BrckOpen == BrckOpen = True
  BrckClose == BrckClose = True
  TokLit a1 == TokLit a2 = a1 == a2
  TokBinOp _ b1 c1 d1 == TokBinOp _ b2 c2 d2 = b1 == b2 && c1 == c2 && d1 == d2
  _ == _ = False

type OperatorTable a = [(Char, (a -> a -> a, Int, Dir))]

tAdd, tMinus, tMul, tDiv, tPow :: (Floating a) => Tok a
tAdd = TokBinOp (+) '+' 6 InfixL
tMinus = TokBinOp (-) '-' 6 InfixL
tMul = TokBinOp (*) '*' 7 InfixL
tDiv = TokBinOp (/) '/' 7 InfixL
tPow = TokBinOp (**) '^' 8 InfixR

operatorTable :: (Floating a) => OperatorTable a
operatorTable =
    [ ('+', ((+), 6, InfixL))
    , ('-', ((-), 6, InfixL))
    , ('*', ((*), 7, InfixL))
    , ('/', ((/), 7, InfixL))
    , ('^', ((**), 8, InfixR))
    ]

operatorFromChar :: OperatorTable a -> Char -> Maybe (Tok a)
operatorFromChar [] _ = Nothing
operatorFromChar ((a, (b, c, d)) : rs) n 
  | a == n = Just (TokBinOp b a c d)
  | otherwise = operatorFromChar rs n

getOp :: (Floating a) => Char -> Maybe (Tok a)
getOp = operatorFromChar operatorTable

parseTokens :: Read a => OperatorTable a -> String -> Maybe [Tok a]
parseTokens _ [] = Nothing
parseTokens [] _ = Nothing
parseTokens op str 
  | sum (map length (words str)) > genericLength (parseBetter op (words str)) = Nothing
  | otherwise = Just (parseBetter op (words str)) where
  
    parseBetter [] _ = []
    parseBetter _ [] = []
    parseBetter op (('(' : xs) : rs) = BrckOpen : parseBetter op (xs : rs)
    parseBetter op ((')' : xs) : rs) = BrckClose : parseBetter op (xs : rs)
    parseBetter op ((s : []) : rs)
      | Just (TokBinOp a b c d) <- operatorFromChar op s = TokBinOp a b c d : parseBetter op rs
      | Nothing <- operatorFromChar op s, Just t <- rk = TokLit t : parseBetter op rs
      | Nothing <- operatorFromChar op s, Nothing <- rk = [] where
        rk = readMaybe [s]
    parseBetter op (str : rs)
      | Just temp <- rd = temp : parseBetter op rs
      | Nothing <- rd = [] where
        rd = readMaybe str
        
parse :: String -> Maybe [Tok Double]
parse = parseTokens operatorTable
{-
parseAndEval :: (String -> Maybe [Tok a]) -> ([Tok a] -> ([a], [Tok a])) -> String -> Maybe ([a], [Tok a])
parseAndEval parse eval input = maybe Nothing (Just . eval) (parse input)

shuntingYardBasic :: [Tok a] -> ([a], [Tok a])
shuntingYardBasic [] = ([], [])
shuntingYardBasic (x : xs) = (rs1, rs2) where
  rs1 = litek (x : xs)
  rs2 = elim2 (oppok (x : xs)) rs1

  litek [] = []
  litek (x : xs)
    | TokLit a <- x = a : litek xs
    | otherwise = litek xs

  oppok [] = []
  oppok (x : xs)
    | TokBinOp a b c d <- x = TokBinOp a b c d : oppok xs
    | BrckOpen <- x = BrckOpen : oppok xs
    | BrckClose <- x = BrckClose : oppok xs

  elim2 [] _ = []
  elim2 (x : xs) (y : ys) n
    | BrckClose <- x = elim2 xs (n + 1)
    | BrckOpen <- x = elim2 xs (n - 1)
    | n > 0 = elim2 xs n
    | otherwise = x : elim2 xs n
    

syNoEval :: String -> Maybe ([Double], [Tok Double])
syNoEval = parseAndEval parse shuntingYardBasic

syEvalBasic :: String -> Maybe ([Double], [Tok Double])
syEvalBasic = parseAndEval parse (\t -> shuntingYardBasic $ BrckOpen : (t ++ [BrckClose]))
-}
--syEvalPrecedence :: String -> Maybe ([Double], [Tok Double])
--syEvalPrecedence = parseAndEval parse (\t -> shuntingYardPrecedence $ BrckOpen : (t ++ [BrckClose]))

-- eqError-t vedd ki a kommentből, ha megcsináltad az 1 pontos "Hibatípus definiálása" feladatot
-- eqError = 0 -- Mágikus tesztelőnek szüksége van rá, NE TÖRÖLD!

{-
-- Ezt akkor vedd ki a kommentblokkból, ha a 3 pontos "A parser és az algoritmus újradefiniálása" feladatot megcsináltad.
parseAndEvalSafe ::
    (String -> ShuntingYardResult [Tok a]) ->
    ([Tok a] -> ShuntingYardResult ([a], [Tok a])) ->
    String -> ShuntingYardResult ([a], [Tok a])
parseAndEvalSafe parse eval input = either Left eval (parse input)

sySafe :: String -> ShuntingYardResult ([Double], [Tok Double])
sySafe = parseAndEvalSafe
  (parseSafe operatorTable)
  (\ts -> shuntingYardSafe (BrckOpen : ts ++ [BrckClose]))
-}

{-
-- Ezt akkor vedd ki a kommentblokkból, ha az 1 pontos "Függvénytábla és a típus kiegészítése" feladatot megcsináltad.
tSin, tCos, tLog, tExp, tSqrt :: Floating a => Tok a
tSin = TokFun sin "sin"
tCos = TokFun cos "cos"
tLog = TokFun log "log"
tExp = TokFun exp "exp"
tSqrt = TokFun sqrt "sqrt"

functionTable :: (RealFrac a, Floating a) => FunctionTable a
functionTable =
    [ ("sin", sin)
    , ("cos", cos)
    , ("log", log)
    , ("exp", exp)
    , ("sqrt", sqrt)
    , ("round", (\x -> fromIntegral (round x :: Integer)))
    ]
-}

{-
-- Ezt akkor vedd ki a kommentblokkból, ha a 2 pontos "Függvények parse-olása és kiértékelése" feladatot megcsináltad.
syFun :: String -> Maybe ([Double], [Tok Double])
syFun = parseAndEval
  (parseWithFunctions operatorTable functionTable)
  (\t -> shuntingYardWithFunctions $ BrckOpen : (t ++ [BrckClose]))
-}

{-
-- Ezt akkor vedd ki a kommentblokkból, ha minden más feladatot megcsináltál ez előtt.
syComplete :: String -> ShuntingYardResult ([Double], [Tok Double])
syComplete = parseAndEvalSafe
  (parseComplete operatorTable functionTable)
  (\ts -> shuntingYardComplete (BrckOpen : ts ++ [BrckClose]))
-}
