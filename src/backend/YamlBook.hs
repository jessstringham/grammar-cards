-- This file, along with Parsers, can help read a human-generated 
-- yaml files, and 
module YamlBook
( Rule(..)
, Situation(..)
, Concept(..)
, Section(..)
, Book(..)
, Word(..)
, Exception(..)
, WordInfo(..)
, Group(..)
, Examples(..)
) where 

data Rule = Rule { rule :: String
                 , back :: String} deriving (Show, Eq)


data Situation = Situation { situation :: String
                           , front :: String
                           , rules :: [Rule]} deriving (Show, Eq)


data Concept = Concept { concept :: String
                       , wordlist :: [String]
                       , conceptTrait :: [String]
                       , situations :: [Situation]
                       } deriving (Show, Eq)


data Section = Section { section :: String
                       , concepts :: [Concept]
                       } deriving (Show, Eq)


type Book = [Section]


data Word = Word { label :: String
                 , word :: String
                 , translation :: String
                 } deriving (Show, Eq)

data Exception = Exception { situationRef :: String
                           , words :: [Word]
                           } deriving (Show, Eq)

data WordInfo = WordInfo { wordInfo :: [Word]
                         , ruleRef :: String
                         , exceptions :: [Exception]
                         } deriving (Show, Eq)

data Group = Group { sectionRef :: String
                   , conceptRef :: String
                   , wordsInfo :: [WordInfo]
                   } deriving (Show, Eq)

type Examples = [Group]