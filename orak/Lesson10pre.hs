module Lesson10 where

-- Új ötlet, további általánosítások:

-- Emlékezzünk vissza az alábbi korábban definiált függvényekre:
-- sum, product, elem, (++), concat, any
{-
sum []     = 0
sum (x:xs) = x + sum xs

product []     = 1
product (x:xs) = x * product xs

elem _ []     = False
elem e (x:xs) = e == x || elem e xs

[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys
-}

-- Definiáld az any' függvényt, amely ellenőrzi, hogy létezik-e egy elem egy listában, amelyre egy adott tulajdonság teljesül.
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x : xs) = f x || any' f xs
-- Megjegyzés: Hogy lehetne az elem-et megírni az any-vel? (Remélhetőleg egyértelműen látszódik, hogy erősen hasonlítanak egymásra.)

elemViaAny :: Eq a => a -> [a] -> Bool
-- elemViaAny a (b : bs) = a == b || elemViaAny a bs
elemViaAny a bs = any' (== a) bs

-- Definiáld az all' függvényt, amely ellenőrzi, hogy egy adott tulajdonság az összes listaelemre teljesül-e.
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x : xs) = f x && all' f xs

-- Definiáld az or' függvényt, amely egy listányi Bool-t összevagyol.
or' :: [Bool] -> Bool
or' [] = False
or' (b : bs) = b || or' bs

-- Definiáld az and' függvényt, amely egy listányi Bool-t összeésel.
and' :: [Bool] -> Bool
and' [] = True
and' (b : bs) = b && and' bs

-- Mindegyik függvény definíciója gyakorlatilag ugyanúgy néz ki.
-- Valami az üres listára, meg valami a legalább egy elemre. A rekurzió mindegyikben mindig az egy elemmel kevesebb maradék lista.
-- Hogyan lehetne általánosítani ezt az ötletet?
-- Megj.: Emlékezzünk vissza rá, hogy ilyen fajta rekurzió esetén merre zárójeleződött az eredménykifejezés!

-- Próbáljuk meg lépésenként általánosítani.
-- Amikor hiányzik valami, akkor vegyük hozzá a típushoz, meg a definícióhoz.
generalisedIdeaR :: (a -> b -> b) -> b -> [a] -> b
generalisedIdeaR f acc [] = acc
generalisedIdeaR f acc (x : xs) = f x (generalisedIdeaR f acc xs)
-- R -> right jobbra zárójelez, nem műxik végtelenre emiatt
-- Ezt a függvényt hívják foldr-nek!
-- Létezik a foldr' függvény is, amely a foldr-nek a mohó változata és amely a Data.Foldable modulban található.
-- A foldr' NEM MŰKÖDIK végtelen listával!

-- Természetesen nem csak ilyen fajta rekurziónk volt.
-- Emlékezzünk vissza a reverse-re.
{-
reverse = reverseAcc [] where
  reverseAcc acc []     = acc
  reverseAcc acc (x:xs) = reverseAcc (x:acc) xs

Az ötlet használható sum-mal is:
sum' = sumAcc 0 where
  sumAcc acc []     = acc
  sumAcc acc (x:xs) = sumAcc (acc + x) xs

Emlékezzünk, hogy ilyenkor az eredménykifejezés merre van zárójelezve!
Látszik a hasonlóság a függvények között?

Általánosítsuk ennek az ötletét is!
-}
generalisedIdeaL :: (b -> a -> b) -> b -> [a] -> b
generalisedIdeaL _ acc [] = acc
generalisedIdeaL f acc (x : xs) = generalisedIdeaL f (f acc x) xs

-- Ezt a függvényt hívják foldl-nek.
-- Létezik a foldl' függvény is, amely a foldl-nek a mohó változata és amely a Data.Foldable modulban található.
-- foldl helyett a foldl'-t érdemes használni!
-- pl. foldr  (+) 0 [1..100000000] futásidejű hibát okoz, mert túl nagy a kiértékeletlen kifejezés a lustaság miatt.
--     foldl  (+) 0 [1..100000000] szintén.
--     foldl' (+) 0 [1..100000000] viszont kiszámítható lesz, mert az akkumulált paramétert mohón kezeli, kiértékeli,
--                                 nem gyűjtögeti a kiértékeletlen kifejezéseket.

-- Ha megnézzük mindkét fold-ot, akkor lényegében mindkettő listát dolgoz fel.
-- Ha úgy tetszik, akkor ez a lista destruktora (más szavakkal iterátora, rekurzora, eleiminátora; mindegyik (kb) ugyanazt jelenti ebben a világban),
-- mert a függvény feldolgozza a lista összes elemét, szintaktikailag megkülönbözteti az összes listát egymástól.

-- A végtelen listás szabályok ugyanúgy érvényesek foldr és foldl-re is,
-- tehát a foldr tud működni végtelen listán, mert előbb használjuk a műveletet, mint a rekurziót,
-- a foldl nem tud működni végtelen listán, mert akkumulál, gyűjtöget.

-- Definiáljuk újra a fenti függvényeket foldr-et vagy foldl-et használva.

sumViaFold :: Num a => [a] -> a
sumViaFold = foldr (+) 0

productViaFold :: Num a => [a] -> a
productViaFold = foldr (*) 1

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs -- mivel jobbra húz, ezért xs ys esetben ez ys xs sorrendben lesz a listában 
-- ys viselkedik acc-ként (accumulator)

concatViaFold :: [[a]] -> [a]
concatViaFold xs = foldr (+++) [] xs

reverseViaFold :: [a] -> [a]
reverseViaFold xs = foldl (flip (:)) [] xs

elemViaFold :: Eq a => a -> [a] -> Bool
elemViaFold a xs = foldr (\x f -> f || a == x) False xs

orViaFold :: [Bool] -> Bool
orViaFold = foldr (||) False

andViaFold :: [Bool] -> Bool
andViaFold = foldr (&&) True

anyViaFoldr :: (a -> Bool) -> [a] -> Bool
anyViaFoldr f = foldr (\x t -> t || f x) False -- False mert azt feltesszük hogy üres listánál False

allViaFold :: (a -> Bool) -> [a] -> Bool
allViaFold f = foldr (\x t -> t && f x) True

-- Definiáljuk a foldr1' függvényt, amely ugyanazt csinálja, mint a foldr,
-- csak a kezdőértéke a lista első eleme lesz.
-- Ez természetesen feltételezi, hogy a lista nem üres.
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f (x : xs) = foldr f x xs

-- Ugyanígy létezik foldl1 is.
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' = undefined

-- mintaillesztés guardokban BEADHOZ FONTOS
asd :: Num a => Maybe (a -> b) -> a -> Maybe b
asd mf a
  | Just f <- mf = Just $ f a
  | Nothing <- mf = Nothing

-- Ezekkel érdemes definiálni például az alábbi függvényt:
-- Definiáld a maximum' függvényt, amely egy visszaadja egy lista legnagyobb elemét.
-- Feltehető, hogy a lista nem üres és véges.
maximum' :: undefined
maximum' = undefined
-- Megj.: minimum hasonlóan definiálható.

-- Létezik olyan fajta hajtogatás is, amely az egyes részeredményeket is visszaadja eredményként.
-- Ezt magyarul pásztázásnak hívjuk (bár az angol neve (scanning) sokkal értelmesebb).

-- Definiáljuk a jobbról való pásztázást!
-- scanr'' (:) [] "pásztáznivaló" == ["pásztáznivaló", "ásztáznivaló", "sztáznivaló", "ztáznivaló", "táznivaló", "áznivaló", "znivaló", "nivaló", "ivaló", "való", "aló", "ló", "ó", []]
-- scanr'' (+) 0 [1..10] == [55, 54, 52, 49, 45, 40, 34, 27, 19, 10, 0]

scanr'' :: undefined
scanr'' = undefined
-- Az eredeti függvény a scanr.
-- A scanr NEM MŰKÖDIK végtelen listával!

-- A scanr-ből hogyan kaphatjuk vissza a foldr műveletét?
-- Válasz:

-- Definiáljuk a balról való pásztázást!
-- scanl'' (\acc x -> x : acc) [] "pásztáznivaló" == [[], "p", "áp", "sáp", "zsáp", "tzsáp", "átzsáp", "zátzsáp", "nzátzsáp", "inzátzsáp", "vinzátzsáp", "avinzátzsáp", "lavinzátzsáp", "ólavinzátzsáp"]
-- scanl'' (+) 0 [1..10] == [0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
scanl'' :: undefined
scanl'' = undefined
-- Az eredeti függvény a scanl, de létezik scanl' is, amely a mohó változata a scanr-nek.
-- Meglepő módon a scanl működik végtelen listával!
-- A mohó változat is!

-- A scanl-ből hogyan kaphatjuk vissza a foldl műveletét?
-- Válasz:

-- A scan-eknek is létezik az 1-es változata, tehát scanr1 és scanl1.
-- Amire az 1-es változatoknál vigyázni kell, hogy nem lehet különböző típusú a bemeneti lista és az eredménylista,
-- hiszen a kezdőérték a bemeneti lista első eleme.

-- Mire használható a scan?
-- Definiáld a lucas függvényt, amely a Lucas-sorozatot reprezentálja végtelen listaként.
-- Az első elem legyen a 2, a második az 1, a többi elem a két előző összege.
lucas :: [Integer]
lucas = undefined

-- Definiáld az increasingMaximums függvényt, amely egy olyan részlistát ad vissza, amely az első elemtől kezdve az elemeket növekvő sorrendben tartalmazza.
-- increasingMaximums [] == []
-- increasingMaximums [1] == [1]
-- increasingMaximums [1,2] == [1,2]
-- increasingMaximums [1,0] == [1]
-- increasingMaximums [2,1,3] == [2,3]
-- increasingMaximums [2,0,1] == [2]
-- increasingMaximums [1,3,2,1,8,7,0] == [1,3,8]
increasingMaximums :: undefined
increasingMaximums = undefined

-- GONDOLKODÓS FELADAT:
-- Definiáld a foldl-et foldr-rel!
foldlViaFoldr :: ugyanazATípusaMintAFoldlnek
foldlViaFoldr = undefined
