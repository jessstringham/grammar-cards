-- This file, along with YamlParsers, can help read a human-generated 
-- yaml files, and 
module Definitions
( RuleYaml(..)
, SituationYaml(..)
, ConceptYaml(..)
, SectionYaml(..)
, BookYaml(..)
, WordYaml(..)
, ExceptionYaml(..)
, WordInfoYaml(..)
, GroupYaml(..)
, ExamplesYaml(..)
) where 

data RuleYaml = RuleYaml { rule :: String
                         , back :: String} deriving (Show, Eq)


data SituationYaml = SituationYaml { situation :: String
                                   , front :: String
                                   , rules :: [RuleYaml]} deriving (Show, Eq)


data ConceptYaml = ConceptYaml { concept :: String
                               , wordlist :: [String]
                               , conceptTrait :: [String]
                               , situations :: [SituationYaml]
                               } deriving (Show, Eq)


data SectionYaml = SectionYaml { section :: String
                               , concepts :: [ConceptYaml]
                               } deriving (Show, Eq)


type BookYaml = [SectionYaml]


data WordYaml = WordYaml { label :: String
                         , word :: String
                         , translation :: String
                         } deriving (Show, Eq)

data ExceptionYaml = ExceptionYaml { situationRef :: String
                                   , words :: [WordYaml]
                                   } deriving (Show, Eq)

data WordInfoYaml = WordInfoYaml { wordInfo :: [WordYaml]
                                 , ruleRef :: String
                                 , exceptions :: [ExceptionYaml]
                                 } deriving (Show, Eq)

data GroupYaml = GroupYaml { sectionRef :: String
                           , conceptRef :: String
                           , wordsInfo :: [WordInfoYaml]
                           } deriving (Show, Eq)

type ExamplesYaml = [GroupYaml]