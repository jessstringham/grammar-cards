module GrammarCards.Processor.Template
( parseRuleString
) where

import Control.Applicative ((<*>), pure)
import qualified Data.Text.Lazy as TextLazy (Text, pack)
import Data.List (isSuffixOf)

import GrammarCards.Processor.Book
import Text.Parsec.Text.Lazy()
import Text.Parsec (Parsec, ParsecT, Stream, anyChar, char, choice, eof, getState, lookAhead, many1, manyTill, optionMaybe, runParser, string, try, updateState)


lookupWordSetWord :: [Translation] -> WordRef -> Bool -> String
lookupWordSetWord translations name isTranslation =
    if isTranslation then
        translation word_set_word
    else
        word word_set_word
  where lookup_results = filter (\x -> name == wordName x) translations
        word_set_word = case lookup_results of
                            [] -> error (unWordRef name ++ " translation is missing")
                            (a:_) -> a

many1till :: Stream s m t =>
     ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1till matchmany ending = do
    fir <- matchmany
    rest <- manyTill matchmany ending
    return $ fir:rest


replaceString :: String -> String -> String -> String
replaceString removeThis replaceWithThis accum =
    if removeThis `isSuffixOf` accum then
        prefix ++ replaceWithThis
    else
        accum
  where prefix_length = length accum - length removeThis
        prefix = take prefix_length accum

matchReplaceMultipleRule :: [Translation] -> Parsec TextLazy.Text String ()
matchReplaceMultipleRule translations = do
    _ <- char '/'
    replaceThis <- manyTill anyChar (try (string "/"))
    replaceWithThis <- manyTill anyChar (try (string "/"))

    updateState (replaceString replaceThis replaceWithThis)

    return ()    

matchWordText :: [Translation] -> Parsec TextLazy.Text String ()
matchWordText translations = do
    _ <- char '<'
    translateToken <- optionMaybe $ char '_'
    wordText <- manyTill anyChar (try (char '>'))

    let should_translate = case translateToken of
                            (Just _) -> True
                            (Nothing) -> False

    updateState (++ lookupWordSetWord translations (WordRef wordText) should_translate)

    return ()


matchText :: [Translation] -> Parsec TextLazy.Text String ()
matchText _ = do
    allText <- many1till anyChar 
        (try 
            (lookAhead 
                (choice [ string "|"
                        , string "<"
                        , string "/"
                        , eof >> return ""
                        ])))
    updateState (++ allText)
    return ()

parseRuleString' :: [Translation] -> Parsec TextLazy.Text String String
parseRuleString' translations = do
    _ <- many1 $ choice (map try ([matchReplaceMultipleRule, matchWordText, matchText] <*> pure translations))
    getState


parseRuleString :: RawTemplate -> [Translation] -> String
parseRuleString rawTemplate translations = 
    case result of
        (Left a) -> error $ show a
        (Right a) -> a
  where result = runParser (parseRuleString' translations) "" rawTemplate $ TextLazy.pack rawTemplate

testParser :: String
testParser = 
    parseRuleString "test <hi>, <_hi>|a?hey|" [Translation (WordRef "hi") "bye" "hey"]
