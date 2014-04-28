module LanguageCenter.Processor.Template
( parseRuleString
, functionFromExpr
) where

import LanguageCenter.Processor.Book
import Text.Parsec.Text.Lazy
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import qualified Data.Text.Lazy as TextLazy

type Template = [Expr]

data Expr = WordText String
          | PlaceHolder WordRef Bool
          | StringModifier RemoveThis
          deriving (Show)

data RemoveThis = RemoveThis [OptionalChar] deriving (Show)

data OptionalChar = OptionalChar Char Bool deriving (Show)

matchWordText :: Parser Expr
matchWordText = do
    _ <- char '<'
    translateToken <- optionMaybe $ char '_'
    wordText <- manyTill anyChar (try (char '>'))

    let should_translate = case translateToken of
                            (Just _) -> True
                            (Nothing) -> False

    return $ PlaceHolder (WordRef wordText) should_translate

matchMaybeChar :: Parser OptionalChar
matchMaybeChar = do
    charValue <- anyChar
    optionalChar <- optionMaybe $ char '?'

    let is_optional = case optionalChar of
                            (Just _) -> True
                            (Nothing) -> False

    return $ OptionalChar charValue is_optional

matchRemoveRule :: Parser Expr
matchRemoveRule = do
    _ <- char '|'
    maybeChars <- manyTill matchMaybeChar (try (string "|"))

    return $ StringModifier $ RemoveThis maybeChars

many1till :: Parser a -> Parser b -> Parser [a]
many1till matchmany ending = do
    fir <- matchmany
    rest <- manyTill matchmany ending
    return $ fir:rest


matchText :: Parser Expr
matchText = do
    allText <- many1till anyChar 
        (try 
            (lookAhead 
                (choice [ string "|"
                        , string "<"
                        , eof >> return ""
                        ])))
    return $ WordText allText


parseRuleString' :: Parser Template
parseRuleString' = 
    many1 $ choice (map try [matchWordText, matchRemoveRule, matchText])

parseRuleString :: RawTemplate -> Template
parseRuleString rawTemplate = 
    case result of
        (Left a) -> error $ show a
        (Right a) -> a
  where result = parse parseRuleString' "" $ TextLazy.pack rawTemplate




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
removeCharacter (OptionalChar character is_optional) accum
    | last accum == character = init accum
    | is_optional = accum
    | otherwise = error $ "This word doesn't work! " ++ accum




