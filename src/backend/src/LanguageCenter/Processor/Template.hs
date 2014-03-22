module LanguageCenter.Processor.Template
( parseRuleString
, functionFromExpr
) where

import LanguageCenter.Processor.Book


type Template = [Expr]

data Expr = WordText String
          | PlaceHolder WordRef Bool
          | StringModifier RemoveThis
          deriving (Show)

data RemoveThis = RemoveThis [OptionalChar] deriving (Show)

data OptionalChar = OptionalChar Char Bool deriving (Show)



parseRuleString :: RawTemplate -> Template
parseRuleString = parseSentenceString

parseSentenceString :: RawTemplate -> Template
parseSentenceString text = parseSentenceString' text []

maybeAttachWord :: String -> Template -> Template
maybeAttachWord word rest =
    if word == "" then
        rest
    else
        (WordText $ reverse word):rest

parseSentenceString' :: RawTemplate -> String -> Template
parseSentenceString' [] [] = []
parseSentenceString' [] word = maybeAttachWord word []
parseSentenceString' (first:rest) word
    | first == '|' =
            maybeAttachWord word (mod_exprs:parseSentenceString mod_remaining)
    | first == '<' =
            maybeAttachWord word (ph_exprs:parseSentenceString ph_remaining)
    | otherwise =
        parseSentenceString' rest (first:word)
  where (mod_exprs, mod_remaining) = getStringModifier rest
        (ph_exprs, ph_remaining) = getPlaceHolder rest

getPlaceHolder :: RawTemplate -> (Expr, RawTemplate)
getPlaceHolder [] = error "Empty template for placeholder, what am I supposed to do!?"
getPlaceHolder whole@(letter:rest)
    | letter == '_' = getPlaceHolder' rest [] True
    | otherwise = getPlaceHolder' whole [] False

getPlaceHolder' :: RawTemplate -> String -> Bool -> (Expr, RawTemplate)
getPlaceHolder' [] _ _ = error "Empty template! What is placeholder supposed to do?"
getPlaceHolder' (letter:rest) parsedText isTranslation
    | letter == '>' =
        (PlaceHolder (WordRef (reverse parsedText)) isTranslation, rest)
    | otherwise =
        getPlaceHolder' rest (letter:parsedText) isTranslation

getStringModifier :: RawTemplate -> (Expr, RawTemplate)
getStringModifier rest = getRemoveRule rest []

getRemoveRule :: RawTemplate -> [OptionalChar] -> (Expr, RawTemplate)
getRemoveRule [] _ = error "getRemoveRule is missing the rest of its template!"
getRemoveRule [_] _ = error "getRemoveRule is missing the rest of its template!"
getRemoveRule (letter:next:rest) parsedText
    | letter == '|' =
        (StringModifier (RemoveThis $ reverse parsedText), rest)
    | next == '?' =
            getRemoveRule rest opt_parsed_text
    | otherwise =
            getRemoveRule (next:rest) req_parsed_text
  where opt_parsed_text = OptionalChar letter True:parsedText
        req_parsed_text = OptionalChar letter False:parsedText

extractWord :: WordString -> String
extractWord (Word wordStringWord) = wordStringWord
extractWord errorMessage = error "Undefined! " ++ show errorMessage

lookupWordSetWord :: [WordInfo] -> WordRef -> Bool -> String
lookupWordSetWord ws name isTranslation =
    extractWord
        (if isTranslation then
            translation word_set_word
        else
            word word_set_word)
  where word_set_word = head (filter (\x -> name == wordName x) ws)

-- This takes a template and returns a function that will convert a
-- wordset to a string

functionFromExpr :: Template -> [WordInfo] -> String
functionFromExpr a b = functionFromExpr' a b ""

functionFromExpr' :: Template -> [WordInfo] -> String -> String
functionFromExpr' [] _ accum = accum
functionFromExpr' (PlaceHolder name bool:rest) wordset accum =
    accum ++ functionFromExpr' rest wordset (lookupWordSetWord wordset name bool)
functionFromExpr' (StringModifier removeThis:rest) wordset accum =
    functionFromExpr' rest wordset $ removeSomeCharacters removeThis accum
functionFromExpr' (WordText text:rest) wordset accum =
    functionFromExpr' rest wordset (accum ++ text)

removeSomeCharacters :: RemoveThis -> String -> String
removeSomeCharacters (RemoveThis removeThis) = removeSomeCharacters' removeThis

removeSomeCharacters' :: [OptionalChar] -> String -> String
removeSomeCharacters' [] accum = accum
removeSomeCharacters' optionalChars accum =
    foldl (flip removeCharacter) accum optionalChars

removeCharacter :: OptionalChar -> String -> String
removeCharacter (OptionalChar char optional) accum
    | last accum == char = init accum
    | optional = accum
    | otherwise = error $ "This word doesn't work! " ++ accum

tests :: String
tests =
    let example1 = [WordInfo (WordRef "noun") (Word "spelar") (Word "car")] in
    let template1 = [PlaceHolder (WordRef "noun") False, StringModifier (RemoveThis [])] in
    --let template1 = [WordText "jag "
    --                , PlaceHolder "noun" False
    --                , StringModifier (RemoveThis [OptionalChar 'r' True])] in
    functionFromExpr template1 example1

