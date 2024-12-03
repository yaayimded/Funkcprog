import Text.Read (readMaybe)

parseTokens :: Read a => OperatorTable a -> String -> Maybe [Tok a]
parseTokens op str = parseWords (words str) -- Szavakra bontás (üres szóközök eltüntetése)
  where
    parseWords [] = Just [] -- Üres lista esetén visszaadunk egy üres listát
    parseWords (w:ws) = case parseWord w of
        Nothing -> Nothing -- Ha egy szó hibás, akkor az egész feldolgozás megszakad
        Just tokens -> case parseWords ws of
            Nothing -> Nothing -- Ha a maradék szavak feldolgozása hibás
            Just rest -> Just (tokens ++ rest) -- Összefűzzük a tokeneket

    parseWord :: Read a => String -> Maybe [Tok a]
    parseWord [] = Just [] -- Üres szó esetén üres listát adunk vissza
    parseWord ('(':xs) = case parseWord xs of
        Nothing -> Nothing -- Hibás további feldolgozás esetén
        Just rest -> Just (BrckOpen : rest) -- Helyes feldolgozás
    parseWord (')':xs) = case parseWord xs of
        Nothing -> Nothing -- Hibás további feldolgozás esetén
        Just rest -> Just (BrckClose : rest) -- Helyes feldolgozás
    parseWord [c] -- Egyetlen karakter feldolgozása
      | Just tok <- operatorFromChar op c = Just [tok] -- Operátor esetén token
      | Just lit <- readMaybe [c] = Just [TokLit lit] -- Literál esetén token
      | otherwise = Nothing -- Egyébként hibás bemenet
    parseWord w -- Több karakteres szó feldolgozása
      | Just lit <- readMaybe w = Just [TokLit lit] -- Ha számként olvasható
      | otherwise = Nothing -- Egyébként hibás bemenet
