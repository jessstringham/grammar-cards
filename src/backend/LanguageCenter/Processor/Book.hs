module LanguageCenter.Processor.Book
( Book(..)
, Concept(..)
, Card(..)
, CardBack(..)
, CardBackTemplateFun(..)
, CardFront(..)
, CardFrontTemplateFun(..)
, CardGenerator(..)
, Example(..)
, Exception(..)
, Rule(..)
, RuleRef(..)
, Situation(..)
, SituationRef(..)
, RawTemplate(..)
, RuleApplication(..)
, RuleTemplate(..)
, TemplateFun(..)
, Translation(..)
, WordRef(..)
, compareSituationRef
, compareRuleRef
, emptyConcept
, getRuleRef
, getSituationRef )
where

-- typeclasses
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
    , cardTags :: ![String]
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

data Translation = Translation
    { wordName :: !WordRef
    , word :: !String
    , translation :: !String
    } deriving (Show, Eq)

{- WORDS -}

data Exception = Exception
    { situationRules :: !SituationRef
    , newFront :: !String
    , newBack :: !String
    } deriving (Show, Eq)

instance ContainsSituationRef Exception where
    getSituationRef = situationRules


-- In this situation, apply this rule
data RuleApplication = RuleApplication
    { raSituationRef :: !SituationRef
    , raRuleRef :: !RuleRef
    } deriving (Show, Eq)

instance ContainsRuleRef RuleApplication where
    getRuleRef = raRuleRef

instance ContainsSituationRef RuleApplication where
    getSituationRef = raSituationRef


data Example = Example
    { translations :: ![Translation]
    , eRules :: ![RuleApplication]
    , exceptions :: ![Exception]
    , eRuleTemplates :: ![String]
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

data RuleTemplate = RuleTemplate
    { ruleTemplateName :: !String
    , ruleApplications :: ![RuleApplication] 
    } deriving (Show, Eq)


data Concept = Concept
    { concept :: !String
    , section :: !String
    , situations :: ![Situation]
    , rules :: ![Rule]
    , ruleTemplates :: ![RuleTemplate]
    , requiredWords :: ![String]
    , examples :: ![Example]
    } deriving (Show, Eq)

emptyConcept :: Concept
emptyConcept = Concept "" "" [] [] [] [] []

type Book = [Concept]


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

    print concept1
    --putStrLn $ show concept1