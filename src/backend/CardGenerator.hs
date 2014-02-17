module CardGenerator
( getAllCards
, getConceptCards
, buildCard
, applyCardGenerators
) where

import Book
import Expr
import Template
import Debug.Trace
import Control.Exception.Base

-- todo add tags and stuff
data Card = Card { cardfront :: String
                 , cardback :: String } deriving (Show, Eq)

data CardGenerator = CardGenerator { generator :: [WordInfo] -> Card
					 			   , cardRuleRef :: String 
					 			   , cardSituation :: String }
instance Show CardGenerator where
	show x = cardRuleRef x ++ cardSituation x


{-
Right now, if we run this, we see
	defaultTranslation,enTranslation,ettTranslation,enTranslation,ettTranslation,orTranslation,arTranslation,erTranslation,ett enTranslation,XTranslation,defaultIndefinite Article,enIndefinite Article,ettIndefinite Article,enIndefinite Article,ettIndefinite Article,orIndefinite Article,arIndefinite Article,erIndefinite Article,ett enIndefinite Article,XIndefinite Article,defaultDefinite Article,enDefinite Article,ettDefinite Article,enDefinite Article,ettDefinite Article,o
so it is pairing up unrelated things

-}

cardGeneratorFunction :: TemplateFun -> TemplateFun -> [WordInfo] -> Card
cardGeneratorFunction (Template frontTemplate) (Template backTemplate) wordinfo =
	(Card
		((functionFromExpr $ parseSentenceString $ frontTemplate) wordinfo)
		((functionFromExpr $ parseSentenceString $ backTemplate) wordinfo))

cardGeneratorGenerator :: Situation -> Rule -> CardGenerator
cardGeneratorGenerator situation rule =
	-- parse the sentences
	CardGenerator 
		(cardGeneratorFunction (front situation) (back rule))
		(ruleName rule)
		(situationName situation)



buildCardGenerator :: Situation -> [Rule] -> [CardGenerator]
buildCardGenerator situation rules =
    -- first, filter out the rules we don't care about
    let filteredRules = filter (\r -> (situationName situation) == (situationRef r)) rules in
    -- then create the card generators
    map (cardGeneratorGenerator situation) filteredRules

filterCardsByRule :: String -> [CardGenerator] -> [CardGenerator]
filterCardsByRule ruleref cardGens =
	filter (\c -> cardRuleRef c == ruleref) cardGens


-- todo
applyCardGeneratorsToExample :: [CardGenerator] -> Example -> [Card]
applyCardGeneratorsToExample cardgenerators example =
	let rr = ruleRef example in 
	map (\c -> (generator c) (wordSet example)) (filterCardsByRule rr cardgenerators)
    -- and update these with example's exceptions


applyCardGenerators :: [CardGenerator] -> [Example] -> [Card]
applyCardGenerators cardgenerators examples =
	concat (map (applyCardGeneratorsToExample cardgenerators) examples)

buildCard :: [Situation] -> [Rule] -> [Example] -> [Card]
buildCard situations rules examples =
    -- Here we go.
    -- step one, attach rules to situations
    let cardGenerators = concat $ map (\s -> buildCardGenerator s rules) situations in
    -- now get a list of cards
    applyCardGenerators cardGenerators examples

getConceptCards :: Concept -> [Card]
getConceptCards concept = buildCard (situations concept) (rules concept) (examples concept)

getAllCards :: Book -> [Card]
getAllCards book = concat $ map getConceptCards book

