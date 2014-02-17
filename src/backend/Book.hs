module Book
( WordSet(..)
, WordInfo(..)
, WordString(..)
) where

import Data.List

type WordName = String

{-
This will either have a template we can interpret and apply the rule to a wordset, 
or refer to the Default function, or point out that it is undefined.
-} 
data TemplateFun = TemplateUndefined 
                 | DefaultTemplate
                 | Template String
                 deriving (Show, Eq)


type Book = [Concept]

data Concept = Concept { concept :: String
                       , section :: String
                       , situations :: [Situation]
                       , rules :: [Rule]
                       , requiredWords :: [WordName]
                       , words :: [WordInfo]
                       } deriving (Show, Eq)

emptyConcept :: Concept
emptyConcept = Concept [] [] [] []

-- Should I use Maybe here?
data WordString = Word String | Undefined deriving (Show, Eq)

data WordInfo = WordInfo { wordName :: WordName
                         , word :: WordString
                         , translation :: WordString
                         } deriving (Show, Eq)

type WordSet = [WordInfo]

newWordSetFromConcept :: Concept -> WordSet
newWordSetFromConcept concept =
    map (\n -> WordInfo n Undefined Undefined) (requiredWords concept)

checkForNoUndefined :: WordInfo -> Bool
checkForNoUndefined wordInfo = 
    ((word wordInfo) /= Undefined) && ((translation wordInfo) /= Undefined)

checkAllWordsForNoUndefined :: WordSet -> Bool
checkAllWordsForNoUndefined wordInfos =
    foldl (\x -> \y -> x && (checkForNoUndefined y)) True wordInfos

data WordRule = WordRule { wordSet :: WordSet
                         , situationRules :: (SituationName, RuleName) 
                         } deriving (Show, Eq)


type RuleName = String
type SituationName = String

data Situation = Situation { situationName :: SituationName
                           , front :: TemplateFun
                           } deriving (Show, Eq)

{-
While rules may share ruleName's (that's how you group them!) and
situationRef's, they shouldn't share both a ruleName and situationRef.
-}
data Rule = Rule { ruleName :: RuleName
                 , situationRef :: SituationName
                 , back :: TemplateFun
                 } deriving (Show, Eq)

-- Returns True if all is well
checkUniqueRules :: [Rule] -> Bool
checkUniqueRules rules = 
    let idList = map (\r -> (ruleName r, situationRef r)) rules in
        idList == nub idList


main :: IO ()
main = 
    let concept1 = emptyConcept {rules = [ Rule{ruleName="hi", situationRef="TestSituation", back=(Template "hi")}], situations = [Situation "TestSituation" (Template "<hi>|a|b|"), Situation "TestSituation2" (Template "<_hi>|a|b|")]} in
    let ruleResult = checkUniqueRules (rules concept1) in
    putStrLn $ show ruleResult
    --putStrLn $ show concept1



