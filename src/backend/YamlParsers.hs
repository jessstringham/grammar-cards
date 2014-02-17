{-# LANGUAGE OverloadedStrings #-}

module YamlParsers
( FromJSON
) where

import Control.Monad
import Control.Applicative
import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import Definitions


instance FromJSON RuleYaml where
    parseJSON (Object v) = RuleYaml <$> 
                         v  .: "rule" <*>
                         v  .: "back"

    parseJSON _          = mzero


instance FromJSON SituationYaml where
    parseJSON (Object v) = SituationYaml <$> 
                         v  .: "situation" <*>
                         v  .: "front" <*>
                         v  .: "rules"

    parseJSON _          = mzero

instance FromJSON ConceptYaml where
    parseJSON (Object v) = ConceptYaml <$> 
                         v  .: "concept" <*>
                         v  .: "wordlist" <*>
                         v  .: "conceptTrait" <*>
                         v  .: "situations"

    parseJSON _          = mzero

instance FromJSON SectionYaml where
    parseJSON (Object v) = SectionYaml <$> 
                         v  .: "section" <*>
                         v  .: "concepts"

    parseJSON _          = mzero

instance FromJSON WordYaml where
    parseJSON (Object v) = WordYaml <$> 
                         v  .: "name" <*>
                         v  .: "word" <*>
                         v  .: "translation"

    parseJSON _          = mzero

instance FromJSON ExceptionYaml where
    parseJSON (Object v) = ExceptionYaml <$> 
                         v  .: "situation" <*>
                         v  .: "wordset"

    parseJSON _          = mzero

instance FromJSON WordInfoYaml where
    parseJSON (Object v) = WordInfoYaml <$> 
                         v  .: "wordset" <*>
                         v  .: "rule" <*>
                         v  .: "exceptions"

    parseJSON _          = mzero


instance FromJSON GroupYaml where
    parseJSON (Object v) = GroupYaml <$> 
                         v  .: "section" <*>
                         v  .: "concept" <*>
                         v  .: "words"

    parseJSON _          = mzero

