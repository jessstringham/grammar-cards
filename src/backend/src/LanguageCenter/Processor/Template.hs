module LanguageCenter.Processor.Template
( parseRuleString
) where

import LanguageCenter.Processor.Book
import Text.Parsec.Text.Lazy
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import qualified Data.Text.Lazy as TextLazy

import Control.Applicative

type Template = [Expr]

data Expr = WordText String
          | PlaceHolder WordRef Bool
          | StringModifier RemoveThis
          deriving (Show)

data RemoveThis = RemoveThis [OptionalChar] deriving (Show)

data OptionalChar = OptionalChar Char Bool deriving (Show)


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
  where lookup_results = filter (\x -> name == wordName x) ws
        word_set_word = case lookup_results of
                            [] -> error (unWordRef name ++ " translation is missing")
                            (a:_) -> a


many1till matchmany ending = do
    fir <- matchmany
    rest <- manyTill matchmany ending
    return $ fir:rest


removeCharacterFunction :: Char -> Bool -> String -> String
removeCharacterFunction character is_optional accum
    | last accum == character = init accum
    | is_optional = accum
    | otherwise = error $ "Requires " ++ pure character ++ ", got " ++ (pure $ last accum)


matchMaybeChar :: [WordInfo] -> Parsec TextLazy.Text String (String -> String)
matchMaybeChar wordinfo = do
    charValue <- anyChar
    optionalChar <- optionMaybe $ char '?'

    let is_optional = case optionalChar of
                            (Just _) -> True
                            (Nothing) -> False

    return $ removeCharacterFunction charValue is_optional


matchRemoveRule :: [WordInfo] -> Parsec TextLazy.Text String ()
matchRemoveRule wordinfo = do
    _ <- char '|'
    maybeCharsFunctions <- manyTill (matchMaybeChar wordinfo) (try (string "|"))

    accum <- getState

    -- compose the functions and apply the result to the string
    updateState (foldr (.) id maybeCharsFunctions)

    return ()


matchWordText :: [WordInfo] -> Parsec TextLazy.Text String ()
matchWordText wordinfo = do
    _ <- char '<'
    translateToken <- optionMaybe $ char '_'
    wordText <- manyTill anyChar (try (char '>'))

    let should_translate = case translateToken of
                            (Just _) -> True
                            (Nothing) -> False

    updateState (++ lookupWordSetWord wordinfo (WordRef wordText) should_translate)

    return ()


matchText :: [WordInfo] -> Parsec TextLazy.Text String ()
matchText wordinfo = do
    allText <- many1till anyChar 
        (try 
            (lookAhead 
                (choice [ string "|"
                        , string "<"
                        , eof >> return ""
                        ])))
    updateState (++ allText)
    return ()


parseRuleString' :: [WordInfo] -> Parsec TextLazy.Text String String
parseRuleString' wordinfo = do
    many1 $ choice (map try ([matchRemoveRule, matchWordText, matchText] <*> pure wordinfo)) -- matchRemoveRule
    result <- getState
    return result


parseRuleString :: RawTemplate -> [WordInfo] -> String
parseRuleString rawTemplate wordinfo = 
    case result of
        (Left a) -> error $ show a
        (Right a) -> a
  where result = runParser (parseRuleString' wordinfo) "" rawTemplate $ TextLazy.pack rawTemplate


testParser = 
    parseRuleString "test <hi>, <_hi>|a?hey|" [(WordInfo (WordRef "hi") (Word "bye") (Word "hey"))]
