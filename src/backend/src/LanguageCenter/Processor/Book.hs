module LanguageCenter.Processor.Book where

import Data.List
import Control.Arrow

class ContainsSituationRef a where
    getSituationRef :: a -> SituationRef

compareSituationRef :: (ContainsSituationRef a) => (ContainsSituationRef b) => a -> b -> Bool
compareSituationRef a1 a2 = getSituationRef a1 == getSituationRef a2

class ContainsRuleRef a where
    getRuleRef :: a -> RuleRef

compareRuleRef :: (ContainsRuleRef a) => (ContainsRuleRef b) => a -> b -> Bool
compareRuleRef a1 a2 = getRuleRef a1 == getRuleRef a2


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

instance ContainsRuleRef Card where
    getRuleRef = cardRuleRef

instance ContainsSituationRef Card where
    getSituationRef = cardSituationRef

data CardGenerator = CardGenerator
    { generator :: Example -> Maybe Card
    , cardGenRuleRef :: !RuleRef
    , cardGenSituationRef :: !SituationRef
    }

instance Show CardGenerator where
    show x = unRuleRef (cardGenRuleRef x) ++ unSituationRef (cardGenSituationRef x)

instance ContainsRuleRef CardGenerator where
    getRuleRef = cardGenRuleRef

instance ContainsSituationRef CardGenerator where
    getSituationRef = cardGenSituationRef


newtype WordRef = WordRef
    { unWordRef :: String } deriving (Show, Eq)

type RawTemplate = String

{- SHARED -}

data ConceptTrait = TranslateEachWord

-- Add default string here!
data WordString = Word String | Undefined deriving (Show, Eq)

data WordInfo = WordInfo
    { wordName :: !WordRef
    , word :: !WordString
    , translation :: !WordString
    } deriving (Show, Eq)

{- WORDS -}

data Exception = Exception
    { situationRules :: !SituationRef
    , newFront :: !WordString
    , newBack :: !WordString
    } deriving (Show, Eq)

instance ContainsSituationRef Exception where
    getSituationRef = situationRules

data RuleApplication = RuleApplication
    { raSituationRef :: !SituationRef
    , raRuleRef :: !RuleRef
    } deriving (Show, Eq)

instance ContainsRuleRef RuleApplication where
    getRuleRef = raRuleRef

instance ContainsSituationRef RuleApplication where
    getSituationRef = raSituationRef

data Example = Example
    { wordSet :: ![WordInfo]
    , eRules :: ![RuleApplication]
    , exceptions :: ![Exception]
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
    { situationName :: !SituationRef
    , front :: !CardFrontTemplateFun
    } deriving (Show, Eq)

instance ContainsSituationRef Situation where
    getSituationRef = situationName

{-
While rules may share ruleName's (that's how you group them!) and
situationRef's, they shouldn't share both a ruleName and situationRef.
-}


newtype RuleRef = RuleRef
    { unRuleRef :: String } deriving (Show, Eq)

data Rule = Rule
    { ruleName :: !RuleRef
    , situationRef :: !SituationRef
    , back :: !CardBackTemplateFun
    } deriving (Show, Eq)

instance ContainsRuleRef Rule where
    getRuleRef = ruleName

instance ContainsSituationRef Rule where
    getSituationRef = situationRef


data Concept = Concept
    { concept :: !String
    , section :: !String
    , situations :: ![Situation]
    , rules :: ![Rule]
    , requiredWords :: ![String]
    , examples :: ![Example]
    , conceptTraits :: [String]
    } deriving (Show, Eq)

emptyConcept :: Concept
emptyConcept = Concept "" "" [] [] [] [] []

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