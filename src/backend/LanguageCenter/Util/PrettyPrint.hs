module LanguageCenter.Util.PrettyPrint 
( ppBook
, ppExample
, ppCard
, ppRawTemplate)
where

import LanguageCenter.Processor.Book
import Text.PrettyPrint.HughesPJ


removeSpaces :: String -> String
removeSpaces input = [if l == ' ' then '_' else l | l <- input]

ppCard :: Card -> Doc
ppCard card = 
    if is_exceptional then
        basic_card <+> parens (text "exception")
    else
        basic_card
  where cardFront_text = text $ unCardFront (cardFront card)
        cardBack_text = text $ unCardBack (cardBack card)
        situationRef_text = text $ removeSpaces $ unSituationRef (cardSituationRef card)
        ruleRef_text = text $ removeSpaces $ unRuleRef (cardRuleRef card)
        cardTags_text = foldr (<+>) empty (map (text . removeSpaces) (cardTags card))
        is_exceptional = exceptional card
        basic_card = cardFront_text <> char '\t' <> cardBack_text <> char '\t' <> situationRef_text <+> ruleRef_text <+> cardTags_text



-- newtype WordRef = WordRef
--     { unWordRef :: String } deriving (Show, Eq)

ppWordRef :: WordRef -> Doc
ppWordRef wr = text (unWordRef wr)

-- type RawTemplate = String

ppRawTemplate :: RawTemplate -> Doc
ppRawTemplate = text

-- data Translation = Translation
--     { wordName :: !WordRef
--     , word :: !WordString
--     , translation :: !WordString
--     } deriving (Show, Eq)

ppTranslation :: Translation -> Doc
ppTranslation wi = 
    wi_word_name <> colon <+> wi_word <+> parens wi_translation
  where wi_word_name = ppWordRef $ wordName wi
        wi_word = text $ word wi
        wi_translation = text $ translation wi


-- newtype SituationRef = SituationRef
--     { unSituationRef :: String } deriving (Show, Eq)

ppSituationRef :: SituationRef -> Doc
ppSituationRef sr = text $ unSituationRef sr

-- data Exception = Exception
--     { situationRules :: !SituationRef
--     , newFront :: !WordString
--     , newBack :: !WordString
--     } deriving (Show, Eq)

ppException :: Exception -> Doc
ppException ex =
    pp_situation_rules <> colon <+> pp_new_front <+> parens pp_new_back
  where pp_situation_rules = ppSituationRef $ situationRules ex
        pp_new_front = text $ newFront ex
        pp_new_back = text $ newBack ex

-- newtype RuleRef = RuleRef
--     { unRuleRef :: String } deriving (Show, Eq)

ppRuleRef :: RuleRef -> Doc
ppRuleRef rr = text $ unRuleRef rr

-- data RuleApplication = RuleApplication
--     { raSituationRef :: !SituationRef
--     , raRuleRef :: !RuleRef
--     } deriving (Show, Eq)

ppRuleApplication :: RuleApplication -> Doc
ppRuleApplication ra =
    pp_situation_ref <> colon <+> pp_rule_ref
  where pp_situation_ref = ppSituationRef $ raSituationRef ra
        pp_rule_ref = ppRuleRef $ raRuleRef ra


-- data Example = Example
--     { translations :: ![Translation]
--     , eRules :: ![RuleApplication]
--     , exceptions :: ![Exception]
--     } deriving (Show, Eq)

ppExample :: Example -> Doc
ppExample ex =
    text "Words: " $$ nest 4 pp_wordset $$ text "Rules: " $$ nest 4 pp_rule_application $$ text "Exceptions: " $$ nest 2 pp_exceptions
  where pp_wordset = vcat $ map ppTranslation (translations ex)
        pp_rule_application = vcat $ map ppRuleApplication (eRules ex)
        pp_exceptions = vcat $ map ppException (exceptions ex)


-- data TemplateFun = TemplateUndefined
--                  | DefaultTemplate
--                  | Template RawTemplate
--                  deriving (Show, Eq)

ppTemplateFun :: TemplateFun -> Doc
ppTemplateFun (TemplateUndefined) = text "UNDEFINED"
ppTemplateFun (DefaultTemplate) = text "DEFAULT"
ppTemplateFun (Template rt) = text rt


 
-- newtype CardFrontTemplateFun = CardFrontTemplateFun
--     { unCardFrontTemplateFun :: TemplateFun } deriving (Show, Eq)

ppCardFrontTemplateFun :: CardFrontTemplateFun -> Doc
ppCardFrontTemplateFun ct = ppTemplateFun $ unCardFrontTemplateFun ct


-- newtype CardBackTemplateFun = CardBackTemplateFun
--     { unCardBackTemplateFun :: TemplateFun } deriving (Show, Eq)

ppCardBackTemplateFun :: CardBackTemplateFun -> Doc
ppCardBackTemplateFun ct = ppTemplateFun $ unCardBackTemplateFun ct

-- data Situation = Situation
--     { situationName :: !SituationRef
--     , front :: !CardFrontTemplateFun
--     } deriving (Show, Eq)

ppSituation :: Situation -> Doc
ppSituation sit = 
    pp_situation_name <> colon <+> pp_front
  where pp_situation_name = ppSituationRef $ situationName sit
        pp_front = ppCardFrontTemplateFun $ front sit

-- data Rule = Rule
--     { ruleName :: !RuleRef
--     , situationRef :: !SituationRef
--     , back :: !CardBackTemplateFun
--     } deriving (Show, Eq)

ppRule :: Rule -> Doc
ppRule rule =
    pp_situationref <+> colon <+> pp_rulename $$ nest 4 pp_back
  where pp_rulename = ppRuleRef $ ruleName rule
        pp_situationref = ppSituationRef $ situationRef rule
        pp_back = ppCardBackTemplateFun $ back rule


-- data Concept = Concept
--     { concept :: !String
--     , section :: !String
--     , situations :: ![Situation]
--     , rules :: ![Rule]
--     , requiredWords :: ![String]
--     , examples :: ![Example]
--     , conceptTraits :: [String]
--     } deriving (Show, Eq)

ppConcept :: Concept -> Doc
ppConcept con =
    pp_concept <+> colon <+> pp_section
    $$ nest 4 (
        text "Situations:" $$ nest 4 pp_situations
        $$ text "Rules:" $$ nest 4 pp_rules
        $$ text "Required Words:" $$ nest 4 pp_requiredwords
        $$ text "Examples:" $$ nest 4 pp_examples
        )
  where pp_concept = text $ concept con
        pp_section = text $ section con
        pp_situations = vcat $ map ppSituation $ situations con
        pp_rules = vcat $ map ppRule $ rules con
        pp_requiredwords = vcat $ map text $ requiredWords con
        pp_examples = vcat $ map (\c -> text "- " <> nest 4 (ppExample c)) $ examples con

ppBook :: Book -> Doc
ppBook book =
    vcat (map (\b -> text "-" $$ nest 2 (ppConcept b) ) book)


testplace :: Doc
testplace =
    ppBook sample_book
  where sample_wordref = WordRef "wordref"
        sample_wordstring = "word"
        sample_wordinfo = Translation sample_wordref sample_wordstring sample_wordstring
        sample_ruleref = RuleRef "ruleref"
        sample_situationref = SituationRef "situationref"
        sample_ruleapplication = RuleApplication sample_situationref sample_ruleref
        sample_exception = Exception sample_situationref "front" "back"
        sample_example = Example [sample_wordinfo, sample_wordinfo] [sample_ruleapplication, sample_ruleapplication] [sample_exception, sample_exception] ["Sample template"]
        sample_templatefun = Template "template -> fun"
        sample_cardfronttemplatefun = CardFrontTemplateFun $ Template "card_front_template"
        sample_cardbacktemplatefun = CardBackTemplateFun $ Template "card_back_template"
        sample_situation = Situation sample_situationref sample_cardfronttemplatefun
        sample_rule = Rule sample_ruleref sample_situationref sample_cardbacktemplatefun
        sample_rule_template = RuleTemplate "template name" [sample_ruleapplication]
        sample_concept = Concept "concept" "section" [sample_situation] [sample_rule] [sample_rule_template] ["requiredword"] [sample_example]
        sample_book = [sample_concept, sample_concept]
