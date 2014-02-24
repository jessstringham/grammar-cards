module CardGenerator
( getAllCards
, getConceptCards
, buildCard
, applyCardGenerators
) where

import Book
import Expr
import Template
import Control.Exception.Base

-- todo add tags and stuff
data Card = Card 
    { cardfront :: String
    , cardback :: String } deriving (Show, Eq)

data CardGenerator = CardGenerator 
    { generator :: [WordInfo] -> Card
    , cardRuleRef :: String 
    , cardSituation :: String }
instance Show CardGenerator where
    show x = cardRuleRef x ++ cardSituation x


cardGeneratorFunction :: TemplateFun -> TemplateFun -> [WordInfo] -> Card
cardGeneratorFunction (Template frontTemplate) (Template backTemplate) wordinfo =
    Card
        ((functionFromExpr $ parseSentenceString frontTemplate) wordinfo)
        ((functionFromExpr $ parseSentenceString backTemplate) wordinfo)

cardGeneratorGenerator :: Situation -> Rule -> CardGenerator
cardGeneratorGenerator situation rule =
    -- parse the sentences
    CardGenerator 
        (cardGeneratorFunction (front situation) (back rule))
        (ruleName rule)
        (situationName situation)

buildCardGenerator :: Situation -> [Rule] -> [CardGenerator]
buildCardGenerator situation rules =
    -- filter out the rules we don't care about
    -- then create the card generators
    map (cardGeneratorGenerator situation) filtered_rules
  where filtered_rules = filter (\r -> situationName situation == situationRef r) rules

filterCardsByRule :: String -> [CardGenerator] -> [CardGenerator]
filterCardsByRule ruleref = filter (\c -> cardRuleRef c == ruleref)

-- todo
applyCardGeneratorsToExample :: [CardGenerator] -> Example -> [Card]
applyCardGeneratorsToExample cardgenerators example =
    map (\c -> generator c (wordSet example)) (filterCardsByRule (ruleRef example) cardgenerators)
    -- and update these with example's exceptions

applyCardGenerators :: [CardGenerator] -> [Example] -> [Card]
applyCardGenerators cardgenerators =
    concatMap (applyCardGeneratorsToExample cardgenerators)

buildCard :: [Situation] -> [Rule] -> [Example] -> [Card]
buildCard situations rules =
    -- Here we go.
    -- step one, attach rules to situations
    -- now get a list of cards
    applyCardGenerators card_generators
  where card_generators = concatMap (`buildCardGenerator` rules) situations

getConceptCards :: Concept -> [Card]
getConceptCards concept = buildCard (situations concept) (rules concept) (examples concept)

getAllCards :: Book -> [Card]
getAllCards = concatMap getConceptCards