module CardGenerator
( getAllCards
, getConceptCards
, buildCard
, applyCardGenerators
, printCard
) where

import Control.Exception.Base()
import Data.Maybe

import Book
import Expr
import Template

-- todo add tags and stuff
data Card = Card 
    { cardFront :: String
    , cardBack :: String 
    , cardRuleRef :: String
    , cardSituationRef :: String } deriving (Show, Eq)

data CardGenerator = CardGenerator 
    { generator :: [WordInfo] -> Card
    , cardGenRuleRef :: String 
    , cardGenSituationRef :: String }
instance Show CardGenerator where
    show x = cardGenRuleRef x ++ cardGenSituationRef x

printCard :: Card -> String
printCard card =
    cardFront card ++ "    " ++ cardBack card

-- TODO: Default template
handleCardSide :: TemplateFun -> [WordInfo] -> String
handleCardSide TemplateUndefined _ = error "This side is undefined"
handleCardSide DefaultTemplate _ = error "Not Implemented!"
handleCardSide (Template template) wordinfo =
    functionFromExpr (parseRuleString template) wordinfo


-- why are the types like this? I do not know, something if flipped somewhere.
cardGeneratorFunction :: TemplateFun -> TemplateFun -> String -> String -> [WordInfo] -> Card
cardGeneratorFunction backTemplate frontTemplate ruleRefCardGen situationRefCardGen wordinfo =
    Card
        { cardFront=handleCardSide frontTemplate wordinfo
        , cardBack=handleCardSide backTemplate wordinfo
        , cardRuleRef=ruleRefCardGen
        , cardSituationRef=situationRefCardGen }

cardGeneratorGenerator :: Situation -> Rule -> CardGenerator
cardGeneratorGenerator situation rule =
    -- parse the sentences
    CardGenerator 
        (cardGeneratorFunction (front situation) (back rule) (ruleName rule) (situationName situation))
        (ruleName rule)
        (situationName situation)

buildCardGenerator :: Situation -> [Rule] -> [CardGenerator]
buildCardGenerator situation rawRules =
    -- filter out the rules we don't care about
    -- then create the card generators
    map (cardGeneratorGenerator situation) filtered_rules
  where filtered_rules = filter (\r -> situationName situation == situationRef r) rawRules

filterCardsByRule :: String -> [CardGenerator] -> [CardGenerator]
filterCardsByRule ruleref = filter (\c -> cardGenRuleRef c == ruleref)

requireWord :: WordString -> String
requireWord (Word wordstring) = wordstring
requireWord (Undefined) = error "This field is not defined!"

defaultException :: WordString
defaultException = Word "DEFAULT"

getTranslation :: String -> WordInfo -> WordString
getTranslation oldCardBack wordinfo =
    if allegedTranslation == defaultException then
        Word oldCardBack
    else
        allegedTranslation
  where allegedTranslation = translation wordinfo

applyExceptionToCard :: Exception -> Card -> Card
applyExceptionToCard exception card = 
    card 
        { cardFront = requireWord $ word replacement_word
        , cardBack = requireWord $ getTranslation (cardBack card) replacement_word }
  where replacement_word = replacementWord exception

applyMaybeExceptionToCard :: Card -> Maybe Exception -> Card
applyMaybeExceptionToCard card (Just exception) = 
    applyExceptionToCard exception card
applyMaybeExceptionToCard card (Nothing) = card


filterExceptionsForRule :: Card -> [Exception] -> Maybe Exception
filterExceptionsForRule card allExceptions =
    listToMaybe filtered_exceptions
  where filtered_exceptions = filter ( (==) (cardSituationRef card) . situationRules) allExceptions

maybeApplyExceptionToCard :: [Exception] -> Card -> Card
maybeApplyExceptionToCard allExceptions card =
    applyMaybeExceptionToCard card $ filterExceptionsForRule card allExceptions

-- todo
applyCardGeneratorsToExample :: [CardGenerator] -> Example -> [Card]
applyCardGeneratorsToExample cardGenerators example =
    map (maybeApplyExceptionToCard (exceptions example)) generated_cards
  where generated_cards = map (\c -> generator c (wordSet example)) (filterCardsByRule (ruleRef example) cardGenerators)



applyCardGenerators :: [CardGenerator] -> [Example] -> [Card]
applyCardGenerators cardgenerators =
    concatMap (applyCardGeneratorsToExample cardgenerators)

buildCard :: [Situation] -> [Rule] -> [Example] -> [Card]
buildCard cardSituations cardRules =
    applyCardGenerators card_generators
  where card_generators = concatMap (`buildCardGenerator` cardRules) cardSituations

getConceptCards :: Concept -> [Card]
getConceptCards cardConcept = buildCard (situations cardConcept) (rules cardConcept) (examples cardConcept)

getAllCards :: Book -> [Card]
getAllCards = concatMap getConceptCards
