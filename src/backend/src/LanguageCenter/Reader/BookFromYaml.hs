module LanguageCenter.Reader.BookFromYaml
( buildBook
) where

import qualified Data.Map as Map (empty, findWithDefault, Map)

import LanguageCenter.Processor.Book
import LanguageCenter.Util.Helper (splitList, insertToValueList)
import qualified LanguageCenter.Reader.YamlBook as Yaml

{- Extraction:
    These take in info, the Yaml.*, and returns the *,
    or does something built off of that
-}

extractRule :: SituationRef -> Yaml.Rule -> Rule
extractRule ruleSituationName rawRule =
    Rule rule_name ruleSituationName card_back
  where rule_name = RuleRef (Yaml.ruleName rawRule)
        card_back = CardBackTemplateFun (Template (Yaml.back rawRule))


extractSituationRule :: Yaml.Situation -> (Situation, [Rule])
extractSituationRule rawSituation =
    (Situation sitname card_front, yaml_rules)
  where sitname = SituationRef (Yaml.situation rawSituation)
        card_front = CardFrontTemplateFun (Template $ Yaml.front rawSituation)
        yaml_rules = map (extractRule sitname) (Yaml.rules rawSituation)


extractSituationsRules :: Yaml.Concept -> ([Situation], [Rule])
extractSituationsRules rawConcept =
    splitList situation_list
  where situation_list = map extractSituationRule $ Yaml.situations rawConcept


extractRuleTemplate :: Yaml.RuleTemplate -> RuleTemplate
extractRuleTemplate rule_template =
    RuleTemplate rule_name rule_applications
  where rule_name = Yaml.situationTemplateRef rule_template
        rule_applications = map extractRuleApplication $ Yaml.cards rule_template


extractRuleTemplates :: Yaml.Concept -> [RuleTemplate]
extractRuleTemplates rawConcept =
    map extractRuleTemplate $ Yaml.situationTemplates rawConcept
        

extractConcept :: String -> Yaml.Concept -> Concept
extractConcept sectionName rawConcept =
    emptyConcept { section=sectionName
                 , concept=concept_name
                 , requiredWords=required_words
                 , ruleTemplates=rule_templates
                 , situations=yaml_situations
                 , rules=yaml_rules}
  where concept_name = Yaml.concept rawConcept
        (yaml_situations, yaml_rules) = extractSituationsRules rawConcept
        rule_templates = extractRuleTemplates rawConcept
        required_words = extractRequiredWords rawConcept

extractRequiredWords :: Yaml.Concept -> [String]
extractRequiredWords = Yaml.wordlist

extractException :: Yaml.Exception -> Exception
extractException rawException =
    Exception exception_situation_ref new_front new_back
  where exception_situation_ref = SituationRef (Yaml.situationRef rawException)
        new_front = Yaml.newFront rawException
        new_back = Yaml.newBack rawException

extractExceptions :: [Yaml.Exception] -> [Exception]
extractExceptions = map extractException

extractWord :: Yaml.Word -> Translation
extractWord rawWord =
    Translation word_label word_text word_translation
  where word_label = WordRef $ Yaml.label rawWord
        word_text = Yaml.word rawWord
        word_translation = Yaml.translation rawWord

extractWordSet :: [Yaml.Word] -> [Translation]
extractWordSet = map extractWord


extractRuleApplication :: Yaml.RuleApplication -> RuleApplication
extractRuleApplication c = RuleApplication (SituationRef $ Yaml.raSituationRef c) (RuleRef $ Yaml.raRuleRef c)

extractExample :: Yaml.Example -> Example
extractExample rawExample =
    Example example_word_set example_rule_ref example_exceptions example_rule_templates
  where example_word_set = extractWordSet $ Yaml.translations rawExample
        example_rule_ref = map extractRuleApplication $ Yaml.ruleRefs rawExample
        example_exceptions = extractExceptions $ Yaml.exceptions rawExample
        example_rule_templates = Yaml.ruleTemplates rawExample

extractExamples :: Yaml.Group -> [Example]
extractExamples rawGroup =
    map extractExample $ Yaml.examples rawGroup

-- returns (section, concept) pair and a list of exceptions
extractWordInfo :: Yaml.Group -> ((String, String), [Example])
extractWordInfo rawGroup =
    ((group_concept_ref, group_section_ref), group_examples)
  where group_section_ref = Yaml.sectionRef rawGroup
        group_concept_ref = Yaml.conceptRef rawGroup
        group_examples = extractExamples rawGroup

extractExampleDict :: [Yaml.Group] -> ExampleDict
extractExampleDict =
    foldr (uncurry insertToValueList . extractWordInfo) Map.empty

{- WordList:
    These help us update the book with examples
-}
-- We use this to help us carry around the examples
type ExampleDict = Map.Map (String, String) [Example]


lookupTemplateRules :: [RuleTemplate] -> String -> [RuleApplication]
lookupTemplateRules (r:otherRuleTemplates) templateName
    | ruleTemplateName r == templateName = ruleApplications r
    | otherwise = lookupTemplateRules otherRuleTemplates templateName
lookupTemplateRules [] templateName = error ("Don't know about no " ++ templateName)


updatedExampleWithConcept :: Concept -> Example -> Example
updatedExampleWithConcept concept example =
    example { eRules = new_rules }
  where rule_templates = eRuleTemplates example -- we need to look these up
        old_rules = eRules example
        new_rules = old_rules ++ concatMap (lookupTemplateRules $ ruleTemplates concept) rule_templates


updateWordList :: ExampleDict -> Concept -> Concept
updateWordList exampleDict conceptInput =
    conceptInput { examples = processed_examples }
  where relevant_examples = Map.findWithDefault [] (concept conceptInput, section conceptInput) exampleDict
        processed_examples = map (updatedExampleWithConcept conceptInput) relevant_examples


updateBookWithExamples :: ExampleDict -> Book -> Book
updateBookWithExamples exampleDict =
    map (updateWordList exampleDict)


buildSection :: Yaml.Section -> [Concept]
buildSection rawSection =
    map (extractConcept $ Yaml.section rawSection) (Yaml.concepts rawSection)

buildBook :: Yaml.Book -> Yaml.Examples -> Book
buildBook rawBook rawExample =
    updateBookWithExamples (extractExampleDict rawExample) (concatMap buildSection rawBook)
