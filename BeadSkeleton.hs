module NagyBead where

import Data.Either
import Data.Maybe
import Text.Read(readMaybe)
import Data.List(genericLength)

basicInstances = 0 -- Mágikus tesztelőnek kell ez, NE TÖRÖLD!

data Dir = InfixL | InfixR deriving (Show, Eq, Ord)

data Tok a = BrckOpen | BrckClose | TokLit a | TokBinOp (a -> a -> a) Char Int Dir | TokFun (a -> a) String

instance Show a => Show (Tok a) where
  show BrckOpen = "BrckOpen"
  show BrckClose = "BrckClose"
  show (TokLit a) = "TokLit " ++ show a
  show (TokBinOp _ b c d) = "TokBinOp " ++ show b ++ " " ++ show c ++ " " ++ show d
  show (TokFun _ nev) = "TokFun " ++ nev

instance Eq a => Eq (Tok a) where
  BrckOpen == BrckOpen = True
  BrckClose == BrckClose = True
  TokLit a1 == TokLit a2 = a1 == a2
  TokBinOp _ b1 c1 d1 == TokBinOp _ b2 c2 d2 = b1 == b2 && c1 == c2 && d1 == d2
  TokFun _ nev1 == TokFun _ nev2 = nev1 == nev2
  _ == _ = False

data ShuntingYardError = OperatorOrClosingParenExpected | LiteralOrOpeningParenExpected | NoClosingParen | NoOpeningParen | ParseError deriving (Show, Eq)

type ShuntingYardResult = Either ShuntingYardError

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
parseTokens _ [] = Just []
parseTokens op str = parseBetter (words str) where
  parseBetter [] = Just []
  parseBetter (x : xs) = case egyes x of
    Nothing -> Nothing
    Just stuff -> case parseBetter xs of
      Nothing -> Nothing
      Just rs -> Just (stuff ++ rs)

  egyes [] = Just []
  egyes ('(' : xs) = case egyes xs of
    Nothing -> Nothing
    Just rs -> Just (BrckOpen : rs)
  egyes (')' : xs) = case egyes xs of
    Nothing -> Nothing
    Just rs -> Just (BrckClose : rs)
  egyes [c]
    | Just tok <- operatorFromChar op c = Just [tok]
    | Just lit <- rc = Just [TokLit lit]
    | otherwise = Nothing where
      rc = readMaybe [c]
  egyes str
    | Just lit <- rw = Just [TokLit lit]
    | otherwise = Nothing where
      rw = readMaybe str

parseSafe :: Read a => OperatorTable a -> String -> ShuntingYardResult [Tok a]
parseSafe op str
  | Nothing <- parseTokens op str = Left ParseError 
  | Just sol <- parseTokens op str = Right sol

parse :: String -> Maybe [Tok Double]
parse = parseTokens operatorTable

parseAndEval :: (String -> Maybe [Tok a]) -> ([Tok a] -> ([a], [Tok a])) -> String -> Maybe ([a], [Tok a])
parseAndEval parse eval input = maybe Nothing (Just . eval) (parse input)

shuntingYardBasic :: [Tok a] -> ([a], [Tok a])
shuntingYardBasic [] = ([], [])
shuntingYardBasic toks = syb toks [] [] where
    syb [] lits opps = (lits, opps)
    syb (x : xs) lits opps = case x of
        TokLit a -> syb xs (a : lits) opps
        BrckOpen -> syb xs lits (BrckOpen : opps)
        BrckClose -> let (newLits, newOps) = zaro lits opps in syb xs newLits newOps
        TokBinOp a b c d -> syb xs lits (x : opps)

    zaro lits (BrckOpen : opps) = (lits, opps)
    zaro ( x : y : lits) (TokBinOp a b c d : opps) = zaro (a y x : lits) opps


syNoEval :: String -> Maybe ([Double], [Tok Double])
syNoEval = parseAndEval parse shuntingYardBasic

syEvalBasic :: String -> Maybe ([Double], [Tok Double])
syEvalBasic = parseAndEval parse (\t -> shuntingYardBasic $ BrckOpen : (t ++ [BrckClose]))

shuntingYardPrecedence :: [Tok a] -> ([a], [Tok a])
shuntingYardPrecedence [] = ([], [])
shuntingYardPrecedence toks = syp toks [] [] where
    syp [] lits opps = (lits, opps)
    syp (x : xs) lits opps = case x of
      TokLit a -> syp xs (a : lits) opps
      BrckOpen -> syp xs lits (BrckOpen : opps)
      BrckClose -> let (tempLitek, tempOppok) = zaro lits opps in syp xs tempLitek tempOppok
      TokBinOp a b c d -> let (tempLitek, tempOppok) = opi x lits opps in syp xs tempLitek tempOppok

    zaro lits (BrckOpen : opps) = (lits, opps)
    zaro (x : y : lits) (TokBinOp a _ _ _ : opps) = zaro (a y x : lits) opps
    
    opi (TokBinOp a1 b1 c1 d1) lits ((TokBinOp a2 b2 c2 d2) : opps)
      | kiert c1 d1 c2 d2 = let (x : y : lits') = lits in opi (TokBinOp a1 b1 c1 d1) (a2 y x : lits') opps
    opi x lits opps = (lits, x : opps)

    kiert c1 d1 c2 d2
      | c1 < c2 = True
      | c1 == c2 && d1 == InfixL = True  
      | otherwise = False

shuntingYardSafe :: [Tok a] -> ShuntingYardResult ([a], [Tok a])
shuntingYardSafe [] = Right ([], [])
shuntingYardSafe toks = syp toks [] [] where
    syp [] lits opps
      | van BrckOpen opps = Left NoClosingParen
      | van BrckClose opps = Left NoOpeningParen
      | otherwise = Right (lits, opps)
    syp (x : xs) lits opps
      | TokLit _ <- x, (y : ys) <- xs, TokLit _ <- y = Left OperatorOrClosingParenExpected
      | TokLit _ <- x, (y : ys) <- xs, BrckOpen <- y = Left OperatorOrClosingParenExpected
      | TokLit a <- x = syp xs (a : lits) opps
      | BrckOpen <- x, (y : ys) <- opps, BrckClose <- y = Left LiteralOrOpeningParenExpected
      | BrckOpen <- x, (y : ys) <- xs, TokBinOp _ _ _ _ <- y = Left LiteralOrOpeningParenExpected
      | BrckOpen <- x, (y : ys) <- xs, BrckClose <- y = Left LiteralOrOpeningParenExpected
      | BrckOpen <- x = syp xs lits (BrckOpen : opps)
      | BrckClose <- x, [] <- opps = Left NoOpeningParen
      | BrckClose <- x, (y : ys) <- xs, TokLit _ <- y = Left OperatorOrClosingParenExpected
      | BrckClose <- x, not (van BrckOpen opps) = Left NoOpeningParen
      | BrckClose <- x = let (tempLitek, tempOppok) = zaro lits opps in syp xs tempLitek tempOppok
      | TokBinOp _ _ _ _ <- x, [] <- lits = Left LiteralOrOpeningParenExpected
      | TokBinOp _ _ _ _ <- x, (y : ys) <- xs, TokBinOp _ _ _ _ <- y = Left LiteralOrOpeningParenExpected
      | TokBinOp _ _ _ _ <- x, (y : ys) <- xs, BrckClose <- y = Left LiteralOrOpeningParenExpected
      | TokBinOp a b c d <- x = let (tempLitek, tempOppok) = opi x lits opps in syp xs tempLitek tempOppok

    zaro lits (BrckOpen : opps) = (lits, opps)
    zaro (x : y : lits) (TokBinOp a _ _ _ : opps) = zaro (a y x : lits) opps
    
    opi (TokBinOp a1 b1 c1 d1) lits ((TokBinOp a2 b2 c2 d2) : opps)
      | kiert c1 d1 c2 d2 = let (x : y : lits') = lits in opi (TokBinOp a1 b1 c1 d1) (a2 y x : lits') opps
    opi x lits opps = (lits, x : opps)

    kiert c1 d1 c2 d2
      | c1 < c2 = True
      | c1 == c2 && d1 == InfixL = True  
      | otherwise = False

    van _ [] = False
    van y (x : xs)
      | BrckOpen <- y, BrckOpen <- x = True
      | BrckClose <- y, BrckClose <- x = True
      | otherwise = van y xs

syEvalPrecedence :: String -> Maybe ([Double], [Tok Double])
syEvalPrecedence = parseAndEval parse (\t -> shuntingYardPrecedence $ BrckOpen : (t ++ [BrckClose]))

-- eqError-t vedd ki a kommentből, ha megcsináltad az 1 pontos "Hibatípus definiálása" feladatot
eqError = 0 -- Mágikus tesztelőnek szüksége van rá, NE TÖRÖLD!


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


-- Ezt akkor vedd ki a kommentblokkból, ha az 1 pontos "Függvénytábla és a típus kiegészítése" feladatot megcsináltad.
tSin, tCos, tLog, tExp, tSqrt :: Floating a => Tok a
tSin = TokFun sin "sin"
tCos = TokFun cos "cos"
tLog = TokFun log "log"
tExp = TokFun exp "exp"
tSqrt = TokFun sqrt "sqrt"

type FunctionTable a = [(String, a -> a)]

functionTable :: (RealFrac a, Floating a) => FunctionTable a
functionTable =
    [ ("sin", sin)
    , ("cos", cos)
    , ("log", log)
    , ("exp", exp)
    , ("sqrt", sqrt)
    , ("round", (\x -> fromIntegral (round x :: Integer)))
    ]


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
