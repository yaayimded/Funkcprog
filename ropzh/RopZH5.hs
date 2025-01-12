module RopZH5 where

onlyVowels :: String -> String
onlyVowels [] = []
onlyVowels (x : xs) | isVowel x = x : onlyVowels xs
  | otherwise = onlyVowels xs
  where
  isVowel :: Char -> Bool
  isVowel 'a' = True
  isVowel 'e' = True
  isVowel 'i' = True
  isVowel 'o' = True
  isVowel 'u' = True
  isVowel _ = False  
