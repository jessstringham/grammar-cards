module BookFromYaml
( buildBook
) where

import qualified Data.Map as Map

import Book
import Helper
import qualified YamlBook as Yaml

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

extractConcept :: String -> Yaml.Concept -> Concept
extractConcept sectionName rawConcept =
    emptyConcept { section=sectionName
                 , concept=concept_name
                 , requiredWords=required_words
                 , situations=yaml_situations
                 , rules=yaml_rules}

  where concept_name = Yaml.concept rawConcept
        (yaml_situations, yaml_rules) = extractSituationsRules rawConcept
        required_words = extractRequiredWords rawConcept

extractRequiredWords :: Yaml.Concept -> [String]
extractRequiredWords = Yaml.wordlist

extractException :: Yaml.Exception -> Exception
extractException rawException =
    Exception exception_situation_ref new_front new_back
  where exception_situation_ref = SituationRef (Yaml.situationRef rawException)
        new_front = Word $ Yaml.newFront rawException
        new_back = Word $ Yaml.newBack rawException

extractExceptions :: [Yaml.Exception] -> [Exception]
extractExceptions = map extractException

extractWord :: Yaml.Word -> WordInfo
extractWord rawWord =
    WordInfo word_label word_text word_translation
  where word_label = WordRef $ Yaml.label rawWord
        word_text = Word $ Yaml.word rawWord
        word_translation = Word $ Yaml.translation rawWord

extractWordSet :: [Yaml.Word] -> [WordInfo]
extractWordSet = map extractWord

extractExample :: Yaml.WordInfo -> Example
extractExample rawWordInfo =
    Example example_word_set example_rule_ref example_exceptions
  where example_word_set = extractWordSet $ Yaml.wordInfo rawWordInfo
        example_rule_ref = RuleRef $ Yaml.ruleRef rawWordInfo
        example_exceptions = extractExceptions $ Yaml.exceptions rawWordInfo

extractExamples :: Yaml.Group -> [Example]
extractExamples rawGroup =
    map extractExample $ Yaml.wordsInfo rawGroup

-- returns (section, concept) pair and a list of exceptions
extractWordInfo :: Yaml.Group -> ((String, String), [Example])
extractWordInfo rawGroup =
    ((group_section_ref, group_concept_ref), group_examples)
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

updateWordList :: ExampleDict -> Concept -> Concept
updateWordList exampleDict conceptInput =
    conceptInput { examples = Map.findWithDefault [] (concept conceptInput, section conceptInput) exampleDict }

updateBookWithExamples :: ExampleDict -> Book -> Book
updateBookWithExamples exampleDict =
    map (updateWordList exampleDict)

buildSection :: Yaml.Section -> [Concept]
buildSection rawSection =
    map (extractConcept $ Yaml.section rawSection) (Yaml.concepts rawSection)

{- buildBook:
    just call this one to get yo' book
-}

buildBook :: Yaml.Book -> Yaml.Examples -> Book
buildBook rawBook rawExample =
    updateBookWithExamples (extractExampleDict rawExample) (concatMap buildSection rawBook)
