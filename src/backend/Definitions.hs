
module Definitions
( Rule(..)
, Situation(..)
, Concept(..)
, Section(..)
, Book(..)
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