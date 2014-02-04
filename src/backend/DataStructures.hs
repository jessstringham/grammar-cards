{-# LANGUAGE OverloadedStrings #-}

module DataStructures
( Rule(..)
, Situation(..)
, Concept(..)
, Section(..)
, Book(..)
, FromJSON
) where 

import Control.Monad
import Control.Applicative
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

data Rule = Rule { rule :: String
                 , back :: String} deriving (Show, Eq)

instance FromJSON Rule where
    parseJSON (Object v) = Rule <$> 
                           v  .: "rule" <*>
                           v  .: "back"

    parseJSON _      = mzero



data Situation = Situation { situation :: String
                           , front :: String
                           , rules :: [Rule]} deriving (Show, Eq)

instance FromJSON Situation where
    parseJSON (Object v) = Situation <$> 
                           v  .: "situation" <*>
                           v  .: "front" <*>
                           v  .: "rules"

    parseJSON _      = mzero

data Concept = Concept { concept :: String
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

    parseJSON _      = mzero

data Section = Section { section :: String
                       , concepts :: [Concept]
                       } deriving (Show, Eq)

instance FromJSON Section where
    parseJSON (Object v) = Section <$> 
                           v  .: "section" <*>
                           v  .: "concepts"

    parseJSON _      = mzero


type Book = [Section]