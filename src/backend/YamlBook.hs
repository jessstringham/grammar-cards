{-# LANGUAGE OverloadedStrings #-}
-- This defines 
module YamlBook
( Rule(..)
, Situation(..)
, Concept(..)
, Section(..)
, Book
, Word(..)
, Exception(..)
, WordInfo(..)
, Group(..)
, Examples
, FromJSON
) where 

import Control.Monad
import Control.Applicative
import Data.Yaml

data Rule = Rule 
    { ruleName :: String
    , backText :: String} deriving (Show, Eq)

instance FromJSON Rule where
    parseJSON (Object v) = Rule <$> 
                         v  .: "rule" <*>
                         v  .: "back"

    parseJSON _          = mzero


data Situation = Situation 
    { situation :: String
    , front :: String
    , rules :: [Rule]} deriving (Show, Eq)

instance FromJSON Situation where
    parseJSON (Object v) = Situation <$> 
                         v  .: "situation" <*>
                         v  .: "front" <*>
                         v  .: "rules"

    parseJSON _          = mzero


data Concept = Concept 
    { concept :: String
    , wordlist :: [String]
    , conceptTrait :: [String]
    , situations :: [Situation]
    } deriving (Show, Eq)

instance FromJSON Concept where
    parseJSON (Object v) = Concept <$> 
                         v  .: "concept" <*>
                         v  .: "wordlist" <*>
                         v  .: "conceptTrait" <*>
                         v  .: "situations"

    parseJSON _          = mzero


data Section = Section 
    { section :: String
    , concepts :: [Concept]
    } deriving (Show, Eq)

instance FromJSON Section where
    parseJSON (Object v) = Section <$> 
                         v  .: "section" <*>
                         v  .: "concepts"

    parseJSON _          = mzero


type Book = [Section]


data Word = Word 
    { label :: String
    , word :: String
    , translation :: String
    } deriving (Show, Eq)

instance FromJSON Word where
    parseJSON (Object v) = Word <$> 
                         v  .: "name" <*>
                         v  .: "word" <*>
                         v  .: "translation"

    parseJSON _          = mzero


data Exception = Exception 
    { situationRef :: String
    , words :: [Word]
    } deriving (Show, Eq)

instance FromJSON Exception where
    parseJSON (Object v) = Exception <$> 
                         v  .: "situation" <*>
                         v  .: "wordset"

    parseJSON _          = mzero


-- WordInfo contains exceptions and info about them
data WordInfo = WordInfo 
    { wordInfo :: [Word]
    , ruleRef :: String
    , exceptions :: [Exception]
    } deriving (Show, Eq)

instance FromJSON WordInfo where
    parseJSON (Object v) = WordInfo <$> 
                         v  .: "wordset" <*>
                         v  .: "rule" <*>
                         v  .: "exceptions"

    parseJSON _          = mzero

data Group = Group 
    { sectionRef :: String
    , conceptRef :: String
    , wordsInfo :: [WordInfo]
    } deriving (Show, Eq)

instance FromJSON Group where
    parseJSON (Object v) = Group <$> 
                         v  .: "section" <*>
                         v  .: "concept" <*>
                         v  .: "words"

    parseJSON _          = mzero


type Examples = [Group]