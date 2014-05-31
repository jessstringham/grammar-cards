module GrammarCards.Processor.CardGenerator
( getAllCards
, getConceptCards
, applyCardGenerators
) where


import Control.Applicative ((<*>))
import Data.Maybe (listToMaybe, catMaybes)

import GrammarCards.Processor.Book as Book
import GrammarCards.Processor.Template
import GrammarCards.Util.Helper



handleCardSide :: Book.TemplateFun -> [Book.Translation] -> String
handleCardSide TemplateUndefined _ = error "This side is undefined"
handleCardSide DefaultTemplate _ = error "Not Implemented!"
handleCardSide (Template template) wordinfo =
    parseRuleString template wordinfo


defaultException :: String
defaultException = "DEFAULT"


maybeReplaceCardSide :: String -> String -> String
maybeReplaceCardSide oldCardText replacement =
    if replacement == defaultException then
        oldCardText
    else
        replacement

applyExceptionToCard :: Book.Exception -> Book.Card -> Book.Card
applyExceptionToCard exception card =
    card { cardFront = new_front, cardBack = new_back, exceptional = True }
  where maybe_new_front = newFront exception
        maybe_new_back = newBack exception
        old_front = unCardFront (cardFront card)
        old_back = unCardBack (cardBack card)
        new_front = CardFront (maybeReplaceCardSide old_front maybe_new_front)
        new_back = CardBack (maybeReplaceCardSide old_back maybe_new_back)

-- check through the rule applicators for one that says we should make a card here.
checkCardAndRuleMatch :: Book.Situation -> Book.Rule -> Book.Example -> Bool
checkCardAndRuleMatch situation rule ruleAppls =
    any (\ruleAppl -> compareRuleRef rule ruleAppl
       && Book.compareSituationRef situation ruleAppl ) $ eRules ruleAppls


cardGeneratorFunction :: Book.Situation -> Book.Rule -> Book.Example -> Maybe Book.Card
cardGeneratorFunction situation rule example =
    if is_relevant then
        case applicable_exception of
            Just exception -> Just $ applyExceptionToCard exception card
            Nothing -> Just card
    else
        Nothing
  where is_relevant = checkCardAndRuleMatch situation rule example
        cardFront = (unCardFrontTemplateFun (front situation))
        cardBack = (unCardBackTemplateFun (back rule))
        card = Card
            { cardFront=CardFront (handleCardSide cardFront (translations example))
            , cardBack=CardBack (handleCardSide cardBack (translations example))
            , cardRuleRef=Book.getRuleRef rule
            , cardSituationRef=Book.getSituationRef situation
            , cardTags=Book.eRuleTemplates example
            , exceptional=False}
        applicable_exception = listToMaybe $ filter (Book.compareSituationRef card) (exceptions example)


applyCardGenerators :: [Book.CardGenerator] -> [Book.Example] -> [Book.Card]
applyCardGenerators cardgenerators wordexamples =
    catMaybes $ map Book.generator cardgenerators <*> wordexamples


createCardGenerator :: (Book.Situation, Book.Rule) -> Book.CardGenerator
createCardGenerator (situation, rule) =
    Book.CardGenerator
        (cardGeneratorFunction situation rule)
        (Book.getRuleRef rule)
        (Book.getSituationRef situation)


-- For each combination of situations and rules where the
-- SituationRef matches, create a card generator
buildCardGenerators :: Book.Concept -> [Book.CardGenerator]
buildCardGenerators concept =
    map createCardGenerator situation_rule_combos
  where concept_situations = Book.situations concept
        concept_rules = Book.rules concept
        situation_rule_combos = [
            (situation, rule) 
            | situation <- concept_situations
            , rule <- concept_rules 
            , Book.compareSituationRef situation rule ]


-- go through each concept and create a list of cards
getConceptCards :: Book.Concept -> [Book.Card]
getConceptCards cardConcept = 
    (applyCardGenerators $ cardGenerators) (Book.examples cardConcept)
  where cardGenerators = buildCardGenerators cardConcept

getAllCards :: Book.Book -> [Book.Card]
getAllCards = concatMap getConceptCards
