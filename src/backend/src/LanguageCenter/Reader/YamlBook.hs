{-# LANGUAGE OverloadedStrings #-}
-- This defines
module LanguageCenter.Reader.YamlBook
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
, RuleApplication(..)
) where

import Control.Monad
import Control.Applicative
import Data.Yaml

data Rule = Rule
    { ruleName :: !String
    , back :: !String} deriving (Show, Eq)

instance FromJSON Rule where
    parseJSON (Object v) = Rule <$>
                         v  .: "rule" <*>
                         v  .: "back"

    parseJSON _          = mzero


data Situation = Situation
    { situation :: !String
    , front :: !String
    , rules :: ![Rule]
    } deriving (Show, Eq)

instance FromJSON Situation where
    parseJSON (Object v) = Situation <$>
                         v  .: "situation" <*>
                         v  .: "front" <*>
                         v  .: "rules"

    parseJSON _          = mzero


data Concept = Concept
    { concept :: !String
    , wordlist :: ![String]
    , conceptTraits :: ![String]
    , situations :: ![Situation]
    } deriving (Show, Eq)

instance FromJSON Concept where
    parseJSON (Object v) = Concept <$>
                         v  .: "concept" <*>
                         v  .: "wordlist" <*>
                         liftM (maybe [] id) (v  .:? "conceptTrait") <*>
                         v  .: "situations"

    parseJSON _          = mzero


data Section = Section
    { section :: !String
    , concepts :: ![Concept]
    } deriving (Show, Eq)

instance FromJSON Section where
    parseJSON (Object v) = Section <$>
                         v  .: "section" <*>
                         v  .: "concepts"

    parseJSON _          = mzero


type Book = [Section]


data Word = Word
    { label :: !String
    , word :: !String
    , translation :: !String
    } deriving (Show, Eq)

instance FromJSON Word where
    parseJSON (Object v) = Word <$>
                         v  .: "name" <*>
                         v  .: "word" <*>
                         v  .: "translation"

    parseJSON _          = mzero


data Exception = Exception
    { situationRef :: !String
    , newFront :: !String
    , newBack :: !String
    } deriving (Show, Eq)

instance FromJSON Exception where
    parseJSON (Object v) = Exception <$>
                         v  .: "situation" <*>
                         v  .: "new_front" <*>
                         v  .: "new_back"

    parseJSON _          = mzero


data RuleApplication = RuleApplication
    { raSituationRef :: !String
    , raRuleRef :: !String
    } deriving (Show, Eq)

instance FromJSON RuleApplication where
    parseJSON (Object v) = RuleApplication <$>
                         v  .: "situation" <*>
                         v  .: "rule"

    parseJSON _          = mzero

-- WordInfo contains exceptions and info about them
data WordInfo = WordInfo
    { wordInfo :: ![Word]
    , ruleRefs :: ![RuleApplication]
    , exceptions :: ![Exception]
    } deriving (Show, Eq)

instance FromJSON WordInfo where
    parseJSON (Object v) = WordInfo <$>
                         v  .: "wordset" <*>
                         v  .: "rules" <*>
                         liftM (maybe [] id) (v  .:? "exceptions")

    parseJSON _          = mzero

data Group = Group
    { sectionRef :: !String
    , conceptRef :: !String
    , wordsInfo :: ![WordInfo]
    } deriving (Show, Eq)

instance FromJSON Group where
    parseJSON (Object v) = Group <$>
                         v  .: "section" <*>
                         v  .: "concept" <*>
                         v  .: "words"

    parseJSON _          = mzero


type Examples = [Group]