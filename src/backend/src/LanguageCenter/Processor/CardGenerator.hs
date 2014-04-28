module LanguageCenter.Processor.CardGenerator
( getAllCards
, getConceptCards
, applyCardGenerators
) where

import Control.Exception.Base()
import Control.Applicative
import Data.Maybe
import Debug.Trace

import LanguageCenter.Util.Helper
import LanguageCenter.Util.PrettyPrint
import LanguageCenter.Processor.Book
import LanguageCenter.Processor.Template




handleCardSide :: TemplateFun -> [WordInfo] -> String
handleCardSide TemplateUndefined _ = error "This side is undefined"
handleCardSide DefaultTemplate _ = error "Not Implemented!"
handleCardSide (Template template) wordinfo =
    functionFromExpr (parseRuleString template) wordinfo




applyExceptionToCard :: Exception -> Card -> Card
applyExceptionToCard exception card =
    card { cardFront = new_front, cardBack = new_back, exceptional = True }
  where maybe_new_front = newFront exception
        maybe_new_back = newBack exception
        new_front = CardFront (requireWord $ maybeReplaceCardSide (unCardFront (cardFront card)) maybe_new_front)
        new_back = CardBack (requireWord $ maybeReplaceCardSide (unCardBack (cardBack card)) maybe_new_back)


compareSituationRef :: (ContainsSituationRef a) => (ContainsSituationRef b) => a -> b -> Bool
compareSituationRef a1 a2 = getSituationRef a1 == getSituationRef a2

compareRuleRef :: (ContainsRuleRef a) => (ContainsRuleRef b) => a -> b -> Bool
compareRuleRef a1 a2 = getRuleRef a1 == getRuleRef a2


checkCardAndRuleMatch' :: Rule -> Situation -> Example -> Bool
checkCardAndRuleMatch' rule situation example =
    any (\ruleAppl -> compareRuleRef rule ruleAppl
       && compareSituationRef situation ruleAppl ) $ eRules example

exceptionMatchesSituation :: Card -> Exception -> Bool
exceptionMatchesSituation card = (==) (cardSituationRef card) . situationRules


cardGeneratorFunction :: Situation -> Rule -> Example -> Maybe Card
cardGeneratorFunction situation rule example =
    if is_relevant then
        case applicable_exception of
            Just exception -> Just $ applyExceptionToCard exception card
            Nothing -> Just card
    else
        Nothing
  where is_relevant = checkCardAndRuleMatch' rule situation example
        card = Card
            { cardFront=CardFront (handleCardSide (unCardFrontTemplateFun (front situation)) (wordSet example))
            , cardBack=CardBack (handleCardSide (unCardBackTemplateFun (back rule)) (wordSet example))
            , cardRuleRef=(ruleName rule)
            , cardSituationRef=(situationName situation) 
            , exceptional=False}
        applicable_exception = listToMaybe $ filter (exceptionMatchesSituation card) (exceptions example)


cardGeneratorGenerator :: Situation -> Rule -> CardGenerator
cardGeneratorGenerator situation rule =
    -- parse the sentences
    CardGenerator
        (cardGeneratorFunction situation rule)
        (ruleName rule)
        (situationName situation)


requireWord :: WordString -> String
requireWord (Word wordstring) = wordstring
requireWord (Undefined) = error "This field is not defined!"

defaultException :: WordString
defaultException = Word "DEFAULT"

maybeReplaceCardSide :: String -> WordString -> WordString
maybeReplaceCardSide oldCardText replacement =
    if replacement == defaultException then
        Word oldCardText
    else
        replacement


applyCardGenerators :: [CardGenerator] -> [Example] -> [Card]
applyCardGenerators cardgenerators examples =
    filtered_cards
  where generated_cards = map generator cardgenerators <*> examples
        filtered_cards = catMaybes generated_cards


buildCardGenerator :: Situation -> [Rule] -> [CardGenerator]
buildCardGenerator situation rawRules =
    -- filter out the rules we don't care about
    -- then create the card generators
    map (cardGeneratorGenerator situation) filtered_rules
  where filtered_rules = filter (compareSituationRef situation) rawRules

-- go through each concept and create a list of cards
getConceptCards :: Concept -> [Card]
getConceptCards cardConcept = 
    (applyCardGenerators $ 
        concatMap 
            (`buildCardGenerator` rules cardConcept) (situations cardConcept))
    $ examples cardConcept

getAllCards :: Book -> [Card]
getAllCards = concatMap getConceptCards
