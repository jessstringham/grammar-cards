module Template
( RemoveThis(..)
, OptionalChar(..)
, parseRuleString
, RawTemplate
, Template
, Expr(..)
, WordRef(..)
) where

newtype WordRef = WordRef
    { unWordRef :: String } deriving (Show, Eq)

type Template = [Expr]

data Expr = WordText String
          | PlaceHolder WordRef Bool
          | StringModifier RemoveThis
          deriving (Show)

data RemoveThis = RemoveThis [OptionalChar] deriving (Show)

data OptionalChar = OptionalChar Char Bool deriving (Show)

type RawTemplate = String

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
