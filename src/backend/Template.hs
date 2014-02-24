module Template
( Expr(..)
, RemoveThis(..)
, AddThis(..)
, OptionalChar(..)
, parseSentenceString
, RawTemplate(..)
) where

data Expr = WordText String 
          | PlaceHolder String Bool
          | StringModifier RemoveThis AddThis
          deriving (Show)

data RemoveThis = RemoveThis [OptionalChar] deriving (Show)
data AddThis = AddThis String deriving (Show)

data OptionalChar = OptionalChar Char Bool deriving (Show)

type RawTemplate = String

parseRuleString = parseSentenceString

parseSentenceString :: RawTemplate -> [Expr]
parseSentenceString text = parseSentenceString' text []

maybeAttachWord word rest =
    if word == "" then 
        rest
    else
        (WordText $ reverse word):rest

parseSentenceString' :: RawTemplate -> String -> [Expr]
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
getPlaceHolder whole@(letter:rest)
    | letter == '_' = getPlaceHolder' rest [] True
    | otherwise = getPlaceHolder' whole [] False

getPlaceHolder' :: RawTemplate -> String -> Bool -> (Expr, RawTemplate)
getPlaceHolder' (letter:rest) parsedText translation
    | letter == '>' =
        (PlaceHolder (reverse parsedText) translation, rest)
    | otherwise =
        getPlaceHolder' rest (letter:parsedText) translation

getStringModifier :: RawTemplate -> (Expr, RawTemplate)
getStringModifier rest = getRemoveRule rest []

getRemoveRule :: RawTemplate -> [OptionalChar] -> (Expr, RawTemplate)
getRemoveRule (letter:next:rest) parsedText
    | letter == '|' = 
        (StringModifier (RemoveThis $ reverse parsedText) add_rule, remaining)
    | next == '?' = 
            getRemoveRule rest opt_parsed_text
    | otherwise =
            getRemoveRule (next:rest) req_parsed_text
  where (add_rule, remaining) = getAddRule (next:rest)
        opt_parsed_text = OptionalChar letter True:parsedText
        req_parsed_text = OptionalChar letter False:parsedText

getAddRule :: RawTemplate -> (AddThis, RawTemplate)
getAddRule text = getAddRule' text ""

getAddRule' :: RawTemplate -> String -> (AddThis, RawTemplate)
getAddRule' (letter:rest) parsedTextProgress 
    | letter == '|' =
        (AddThis $ reverse parsedTextProgress, rest)
    | otherwise =
        getAddRule' rest (letter:parsedTextProgress)
