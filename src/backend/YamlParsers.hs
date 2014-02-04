{-# LANGUAGE OverloadedStrings #-}

module YamlParsers
( FromJSON
) where

import Control.Monad
import Control.Applicative
import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import Definitions


instance FromJSON Rule where
    parseJSON (Object v) = Rule <$> 
                           v  .: "rule" <*>
                           v  .: "back"

    parseJSON _      = mzero


instance FromJSON Situation where
    parseJSON (Object v) = Situation <$> 
                           v  .: "situation" <*>
                           v  .: "front" <*>
                           v  .: "rules"

    parseJSON _      = mzero

instance FromJSON Concept where
    parseJSON (Object v) = Concept <$> 
                           v  .: "concept" <*>
                           v  .: "wordlist" <*>
                           v  .: "conceptTrait" <*>
                           v  .: "situations"

    parseJSON _      = mzero

instance FromJSON Section where
    parseJSON (Object v) = Section <$> 
                           v  .: "section" <*>
                           v  .: "concepts"

    parseJSON _      = mzero
