module LanguageCenter.Processor.CardGenerator
( getAllCards
, getConceptCards
, buildCard
, applyCardGenerators
, printCard
) where

import Control.Exception.Base()
import Control.Applicative
import Data.Maybe
import Debug.Trace
import Data.Maybe

import LanguageCenter.Util.Helper
import LanguageCenter.Util.PrettyPrint
import LanguageCenter.Processor.Book
import LanguageCenter.Processor.Template

import Text.PrettyPrint.HughesPJ


ppCard :: Card -> Doc
ppCard card = 
    if is_exceptional then
        basic_card <+> parens (text "exception")
    else
        basic_card

  where cardFront_text = text $ unCardFront (cardFront card)
        cardBack_text = text $ unCardBack (cardBack card)
        situationRef_text = text $ unSituationRef (cardSituationRef card)
        ruleRef_text = text $ unRuleRef (cardRuleRef card)
        is_exceptional = exceptional card
        basic_card = cardFront_text <+> cardBack_text <+> situationRef_text <> colon <> ruleRef_text
    


newtype CardFront = CardFront
    { unCardFront :: String } deriving (Show, Eq)

newtype CardBack = CardBack
    { unCardBack :: String } deriving (Show, Eq)

-- todo add tags and stuff
data Card = Card
    { cardFront :: !CardFront
    , cardBack :: !CardBack
    , cardRuleRef :: !RuleRef
    , cardSituationRef :: !SituationRef
    , exceptional :: !Bool
    } deriving (Show, Eq)

data CardGenerator = CardGenerator
    { generator :: Example -> Maybe Card
    , cardGenRuleRef :: !RuleRef
    , cardGenSituationRef :: !SituationRef
    }
instance Show CardGenerator where
    show x = unRuleRef (cardGenRuleRef x) ++ unSituationRef (cardGenSituationRef x)

printCard :: Card -> String
printCard card =
    --unCardFront (cardFront card) ++ "    " ++ unCardBack (cardBack card) ++ "    " ++ unSituationRef (cardSituationRef card) ++ "-" ++ unRuleRef (cardRuleRef card) ++ "(" ++ (show $ exceptional card) ++ ")"
    show $ ppCard card

handleCardSide :: TemplateFun -> [WordInfo] -> String
handleCardSide TemplateUndefined _ = error "This side is undefined"
handleCardSide DefaultTemplate _ = error "Not Implemented!"
handleCardSide (Template template) wordinfo =
    functionFromExpr (parseRuleString template) wordinfo



checkCardAndRuleMatch' :: [RuleApplication] -> RuleRef -> SituationRef -> Bool
checkCardAndRuleMatch' ruleAppls ruleRef sitRef =
    any (\ruleAppl -> and[ (ruleRef == raRuleRef ruleAppl)
       , (sitRef == raSituationRef ruleAppl) ]) ruleAppls

cardGeneratorFunction :: CardFrontTemplateFun -> CardBackTemplateFun -> RuleRef -> SituationRef -> Example -> Maybe Card
cardGeneratorFunction frontTemplate backTemplate ruleRefCardGen situationRefCardGen example =
    if (checkCardAndRuleMatch' (eRules example) ruleRefCardGen situationRefCardGen) then
        Just Card
            { cardFront=CardFront (handleCardSide (unCardFrontTemplateFun frontTemplate) (wordSet example))
            , cardBack=CardBack (handleCardSide (unCardBackTemplateFun backTemplate) (wordSet example))
            , cardRuleRef=ruleRefCardGen
            , cardSituationRef=situationRefCardGen 
            , exceptional=False}
    else
        Nothing

checkCardAndRuleMatch :: (RuleApplication, CardGenerator) -> Bool
checkCardAndRuleMatch (ruleAppl, cardGen) =
    and[ (cardGenRuleRef cardGen == raRuleRef ruleAppl)
       , (cardGenSituationRef cardGen == raSituationRef ruleAppl) ]


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

applyExceptionToCard :: Exception -> Card -> Card
applyExceptionToCard exception card =
    card { cardFront = new_front, cardBack = new_back, exceptional = True }
  where maybe_new_front = newFront exception
        maybe_new_back = newBack exception
        new_front = CardFront (requireWord $ maybeReplaceCardSide (unCardFront (cardFront card)) maybe_new_front)
        new_back = CardBack (requireWord $ maybeReplaceCardSide (unCardBack (cardBack card)) maybe_new_back)


exceptionMatchesSituation :: Card -> Exception -> Bool
exceptionMatchesSituation card = (==) (cardSituationRef card) . situationRules

applyExceptionToCardIfAny :: [Exception] -> Card -> Card
applyExceptionToCardIfAny allExceptions card =
    case maybe_exception of
        Just exception -> applyExceptionToCard exception card
        Nothing -> card
  where maybe_exception = listToMaybe $ filter (exceptionMatchesSituation card) allExceptions


filterCardGenByRule :: [RuleApplication] -> [CardGenerator] -> [CardGenerator]
filterCardGenByRule ruleAppl cardGenerators =
    map snd $ filter checkCardAndRuleMatch (combinations ruleAppl cardGenerators)

applyCardGenerators :: [CardGenerator] -> [Example] -> [Card]
applyCardGenerators cardgenerators examples =
    finished_cards
  where generated_cards = (map generator cardgenerators) <*> examples
        filtered_cards = catMaybes generated_cards
        finished_cards = map (applyExceptionToCardIfAny (exceptions example)) generated_cards


buildCard :: [Situation] -> [Rule] -> [Example] -> [Card]
buildCard cardSituations cardRules =
    applyCardGenerators $ concatMap (`buildCardGenerator` cardRules) cardSituations

-- go through each concept and create a list of cards
getConceptCards :: Concept -> [Card]
--getConceptCards cardConcept | trace ("myfun " ++ show cardConcept) False = undefined
getConceptCards cardConcept = buildCard (situations cardConcept) (rules cardConcept) (examples cardConcept)


getAllCards :: Book -> [Card]
getAllCards = concatMap getConceptCards
