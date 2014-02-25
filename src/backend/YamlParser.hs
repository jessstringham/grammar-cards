{-# LANGUAGE OverloadedStrings #-}

module YamlParser
( FromJSON
) where

import Control.Monad
import Control.Applicative
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

import qualified YamlBook as Yaml


instance FromJSON Yaml.Rule where
    parseJSON (Object v) = Yaml.Rule <$> 
                         v  .: "rule" <*>
                         v  .: "back"

    parseJSON _          = mzero


instance FromJSON Yaml.Situation where
    parseJSON (Object v) = Yaml.Situation <$> 
                         v  .: "situation" <*>
                         v  .: "front" <*>
                         v  .: "rules"

    parseJSON _          = mzero


instance FromJSON Yaml.Concept where
    parseJSON (Object v) = Yaml.Concept <$> 
                         v  .: "concept" <*>
                         v  .: "wordlist" <*>
                         v  .: "conceptTrait" <*>
                         v  .: "situations"

    parseJSON _          = mzero


instance FromJSON Yaml.Section where
    parseJSON (Object v) = Yaml.Section <$> 
                         v  .: "section" <*>
                         v  .: "concepts"

    parseJSON _          = mzero


instance FromJSON Yaml.Word where
    parseJSON (Object v) = Yaml.Word <$> 
                         v  .: "name" <*>
                         v  .: "word" <*>
                         v  .: "translation"

    parseJSON _          = mzero


instance FromJSON Yaml.Exception where
    parseJSON (Object v) = Yaml.Exception <$> 
                         v  .: "situation" <*>
                         v  .: "wordset"

    parseJSON _          = mzero


instance FromJSON Yaml.WordInfo where
    parseJSON (Object v) = Yaml.WordInfo <$> 
                         v  .: "wordset" <*>
                         v  .: "rule" <*>
                         v  .: "exceptions"

    parseJSON _          = mzero


instance FromJSON Yaml.Group where
    parseJSON (Object v) = Yaml.Group <$> 
                         v  .: "section" <*>
                         v  .: "concept" <*>
                         v  .: "words"

    parseJSON _          = mzero