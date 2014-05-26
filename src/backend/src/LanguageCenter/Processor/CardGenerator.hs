module LanguageCenter.Processor.CardGenerator
( getAllCards
, getConceptCards
, applyCardGenerators
) where

import Control.Exception.Base()
import Control.Applicative
import Data.Maybe

import LanguageCenter.Processor.Book
import LanguageCenter.Processor.Template
import LanguageCenter.Util.Helper



handleCardSide :: TemplateFun -> [WordInfo] -> String
handleCardSide TemplateUndefined _ = error "This side is undefined"
handleCardSide DefaultTemplate _ = error "Not Implemented!"
handleCardSide (Template template) wordinfo =
    parseRuleString template wordinfo


applyExceptionToCard :: Exception -> Card -> Card
applyExceptionToCard exception card =
    card { cardFront = new_front, cardBack = new_back, exceptional = True }
  where maybe_new_front = newFront exception
        maybe_new_back = newBack exception
        new_front = CardFront (requireWord $ maybeReplaceCardSide (unCardFront (cardFront card)) maybe_new_front)
        new_back = CardBack (requireWord $ maybeReplaceCardSide (unCardBack (cardBack card)) maybe_new_back)

-- check through the rule applicators for one that says we should make a card here.
checkCardAndRuleMatch :: Situation -> Rule -> Example -> Bool
checkCardAndRuleMatch situation rule ruleAppls =
    any (\ruleAppl -> compareRuleRef rule ruleAppl
       && compareSituationRef situation ruleAppl ) $ eRules ruleAppls


cardGeneratorFunction :: Situation -> Rule -> Example -> Maybe Card
cardGeneratorFunction situation rule example =
    if is_relevant then
        case applicable_exception of
            Just exception -> Just $ applyExceptionToCard exception card
            Nothing -> Just card
    else
        Nothing
  where is_relevant = checkCardAndRuleMatch situation rule example
        card = Card
            { cardFront=CardFront (handleCardSide (unCardFrontTemplateFun (front situation)) (wordSet example))
            , cardBack=CardBack (handleCardSide (unCardBackTemplateFun (back rule)) (wordSet example))
            , cardRuleRef=getRuleRef rule
            , cardSituationRef=getSituationRef situation
            , exceptional=False}
        applicable_exception = listToMaybe $ filter (compareSituationRef card) (exceptions example)


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
applyCardGenerators cardgenerators wordexamples =
    catMaybes $ map generator cardgenerators <*> wordexamples


cardGeneratorGenerator :: Situation -> Rule -> CardGenerator
cardGeneratorGenerator situation rule =
    -- parse the sentences
    CardGenerator
        (cardGeneratorFunction situation rule)
        (ruleName rule)
        (situationName situation)


buildCardGenerators :: [Situation] -> [Rule] -> [CardGenerator]
buildCardGenerators sits rawRules =
    -- filter out the rules we don't care about
    -- then create the card generators
    map 
        (uncurry cardGeneratorGenerator)
        [(sit, rule) 
        | sit <- sits
        , rule <- rawRules 
        , compareSituationRef sit rule]


-- go through each concept and create a list of cards
getConceptCards :: Concept -> [Card]
getConceptCards cardConcept = 
    (applyCardGenerators $ 
        buildCardGenerators (situations cardConcept) (rules cardConcept))
    (examples cardConcept)

getAllCards :: Book -> [Card]
getAllCards = concatMap getConceptCards
