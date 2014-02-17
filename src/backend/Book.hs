module Book
( WordInfo(..)
, WordString(..)
, Book(..)
, Concept(..)
, emptyConcept
, Situation(..)
, Rule(..)
, TemplateFun(..)
, Example(..)
, Exception(..)
) where

import Template
import Data.List

{- SHARED -}

-- Should I use Maybe here?
data WordString = Word String | Undefined deriving (Show, Eq)

data WordInfo = WordInfo { wordName :: String
                         , word :: WordString
                         , translation :: WordString
                         } deriving (Show, Eq)


{- WORDS -}

data Exception = Exception { situationRules :: String 
                           , replacementWords :: [WordInfo]
                           } deriving (Show, Eq)

data Example = Example { wordSet :: [WordInfo]
                       , ruleRef :: String
                       , exceptions :: [Exception]
                       } deriving (Show, Eq)



{- BOOK -}


{-
This will either have a template we can interpret and apply the rule to a wordset, 
or refer to the Default function, or point out that it is undefined.
-} 
data TemplateFun = TemplateUndefined 
                 | DefaultTemplate
                 | Template RawTemplate
                 deriving (Show, Eq)

data Situation = Situation { situationName :: String
                           , front :: TemplateFun
                           } deriving (Show, Eq)

{-
While rules may share ruleName's (that's how you group them!) and
situationRef's, they shouldn't share both a ruleName and situationRef.
-}
data Rule = Rule { ruleName :: String
                 , situationRef :: String
                 , back :: TemplateFun
                 } deriving (Show, Eq)

data Concept = Concept { concept :: String
                       , section :: String
                       , situations :: [Situation]
                       , rules :: [Rule]
                       , requiredWords :: [String]
                       , examples :: [Example]
                       } deriving (Show, Eq)

emptyConcept :: Concept
emptyConcept = Concept "" "" [] [] [] []

type Book = [Concept]

newWordSetFromStrings :: [String] -> [WordInfo]
newWordSetFromStrings wordlist =
    map (\n -> WordInfo n Undefined Undefined) wordlist

checkForNoUndefined :: WordInfo -> Bool
checkForNoUndefined wordInfo = 
    ((word wordInfo) /= Undefined) && ((translation wordInfo) /= Undefined)

checkAllWordsForNoUndefined :: [WordInfo] -> Bool
checkAllWordsForNoUndefined wordInfos =
    foldl (\x -> \y -> x && (checkForNoUndefined y)) True wordInfos



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



