module Book
( WordInfo(..)
, WordString(..)
, Book
, Concept(..)
, emptyConcept
, Situation(..)
, SituationRef(..)
, Rule(..)
, RuleRef(..)
, TemplateFun(..)
, Example(..)
, Exception(..)
, WordRef(..)
, CardFrontTemplateFun(..)
, CardBackTemplateFun(..)
) where

import Data.List
import Control.Arrow

import Template

{- SHARED -}

-- Should I use Maybe here?
data WordString = Word String | Undefined deriving (Show, Eq)

data WordInfo = WordInfo
    { wordName :: WordRef
    , word :: WordString
    , translation :: WordString
    } deriving (Show, Eq)


{- WORDS -}

data Exception = Exception
    { situationRules :: SituationRef
    , newFront :: WordString
    , newBack :: WordString
    } deriving (Show, Eq)

data Example = Example
    { wordSet :: [WordInfo]
    , ruleRef :: RuleRef
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

newtype CardFrontTemplateFun = CardFrontTemplateFun
    { unCardFrontTemplateFun :: TemplateFun } deriving (Show, Eq)

newtype CardBackTemplateFun = CardBackTemplateFun
    { unCardBackTemplateFun :: TemplateFun } deriving (Show, Eq)

newtype SituationRef = SituationRef
    { unSituationRef :: String } deriving (Show, Eq)

data Situation = Situation
    { situationName :: SituationRef
    , front :: CardFrontTemplateFun
    } deriving (Show, Eq)

{-
While rules may share ruleName's (that's how you group them!) and
situationRef's, they shouldn't share both a ruleName and situationRef.
-}
newtype RuleRef = RuleRef
    { unRuleRef :: String } deriving (Show, Eq)

data Rule = Rule
    { ruleName :: RuleRef
    , situationRef :: SituationRef
    , back :: CardBackTemplateFun
    } deriving (Show, Eq)

data Concept = Concept
    { concept :: String
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
newWordSetFromStrings =
    map (\n -> WordInfo (WordRef n) Undefined Undefined)

checkForNoUndefined :: WordInfo -> Bool
checkForNoUndefined wordInfo =
    (word wordInfo /= Undefined) && (translation wordInfo /= Undefined)

checkAllWordsForNoUndefined :: [WordInfo] -> Bool
checkAllWordsForNoUndefined =
    foldl (\x y -> x && checkForNoUndefined y) True


-- Returns True if all is well
checkUniqueRules :: [Rule] -> Bool
checkUniqueRules rulesToCheck =
    id_list == nub id_list
  where id_list = map (ruleName &&& situationRef) rulesToCheck


main :: IO ()
main =
    let concept1 = emptyConcept {
          rules =
            [ Rule
                { ruleName = RuleRef "hi"
                , situationRef = SituationRef "TestSituation"
                , back=CardBackTemplateFun(Template "hi") }]
        , situations =
            [ Situation (SituationRef "TestSituation") (CardFrontTemplateFun (Template "<hi>|a|b|"))
            , Situation (SituationRef "TestSituation2") (CardFrontTemplateFun (Template "<_hi>|a|b|")) ] } in
    let ruleResult = checkUniqueRules (rules concept1) in
    print ruleResult
    --putStrLn $ show concept1