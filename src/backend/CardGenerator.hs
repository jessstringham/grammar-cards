module CardGenerator
( getAllCards
, getConceptCards
, buildCard
, applyCardGenerators
) where

import Control.Exception.Base()

import Book
import Expr
import Template

-- todo add tags and stuff
data Card = Card 
    { cardFront :: String
    , cardBack :: String } deriving (Show, Eq)

data CardGenerator = CardGenerator 
    { generator :: [WordInfo] -> Card
    , cardRuleRef :: String 
    , cardSituation :: String }
instance Show CardGenerator where
    show x = cardRuleRef x ++ cardSituation x

-- todo Default template!
cardGeneratorFunction :: TemplateFun -> TemplateFun -> [WordInfo] -> Card
cardGeneratorFunction TemplateUndefined _ _ = error "Front is undefined!"
cardGeneratorFunction DefaultTemplate _ _ = error "Not implemented!"
cardGeneratorFunction (Template _) TemplateUndefined _ = error "Back is undefined!"
cardGeneratorFunction (Template _) DefaultTemplate _ = error "Not implemented!"
cardGeneratorFunction (Template frontTemplate) (Template backTemplate) wordinfo =
    Card
        ((functionFromExpr $ parseRuleString frontTemplate) wordinfo)
        ((functionFromExpr $ parseRuleString backTemplate) wordinfo)

cardGeneratorGenerator :: Situation -> Rule -> CardGenerator
cardGeneratorGenerator situation rule =
    -- parse the sentences
    CardGenerator 
        (cardGeneratorFunction (front situation) (back rule))
        (ruleName rule)
        (situationName situation)

buildCardGenerator :: Situation -> [Rule] -> [CardGenerator]
buildCardGenerator situation rawRules =
    -- filter out the rules we don't care about
    -- then create the card generators
    map (cardGeneratorGenerator situation) filtered_rules
  where filtered_rules = filter (\r -> situationName situation == situationRef r) rawRules

filterCardsByRule :: String -> [CardGenerator] -> [CardGenerator]
filterCardsByRule ruleref = filter (\c -> cardRuleRef c == ruleref)

-- todo
applyCardGeneratorsToExample :: [CardGenerator] -> Example -> [Card]
applyCardGeneratorsToExample cardGenerators example =
    map (\c -> generator c (wordSet example)) (filterCardsByRule (ruleRef example) cardGenerators)
    -- and update these with example's exceptions

applyCardGenerators :: [CardGenerator] -> [Example] -> [Card]
applyCardGenerators cardgenerators =
    concatMap (applyCardGeneratorsToExample cardgenerators)

buildCard :: [Situation] -> [Rule] -> [Example] -> [Card]
buildCard cardSituations cardRules =
    -- Here we go.
    -- step one, attach rules to situations
    -- now get a list of cards
    applyCardGenerators card_generators
  where card_generators = concatMap (`buildCardGenerator` cardRules) cardSituations

getConceptCards :: Concept -> [Card]
getConceptCards cardConcept = buildCard (situations cardConcept) (rules cardConcept) (examples cardConcept)

getAllCards :: Book -> [Card]
getAllCards = concatMap getConceptCards