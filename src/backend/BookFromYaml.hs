module BookFromYaml
( buildBook
) where

import qualified Data.Map as Map

import Book
import Helper
import qualified YamlBook as Yaml

extractRule :: String -> Yaml.Rule -> Rule
extractRule name rawRule = 
    Rule (Yaml.rule rawRule) name (Template (Yaml.back rawRule))

extractSituationRule :: Yaml.Situation -> (Situation, [Rule])
extractSituationRule rawSituation =
    (Situation sitname (Template front_card), yaml_rules)
    where sitname = Yaml.situation rawSituation
          front_card = Yaml.front rawSituation
          yaml_rules = map (extractRule sitname) (Yaml.rules rawSituation)

extractSituationsRules :: Yaml.Concept -> ([Situation], [Rule])
extractSituationsRules rawConcept =
    splitList situation_list
  where yaml_situations = Yaml.situations rawConcept
        situation_list = map extractSituationRule yaml_situations

extractRequiredWords :: Yaml.Concept -> [String]
extractRequiredWords = Yaml.wordlist

buildConcept :: String -> Yaml.Concept -> Concept
buildConcept sectionName rawConcept =
    emptyConcept { section=sectionName
                 , concept=conceptName
                 , requiredWords=required_words
                 , situations=yaml_situations
                 , rules=yaml_rules}

  where conceptName = Yaml.concept rawConcept
        (yaml_situations, yaml_rules) = extractSituationsRules rawConcept
        required_words = extractRequiredWords rawConcept

buildSection :: Yaml.Section -> [Concept]
buildSection rawSection =
    map build_this_concept (Yaml.concepts rawSection)
  where build_this_concept = buildConcept $ Yaml.section rawSection

type ExampleDict = Map.Map (String, String) [Example]

extractException :: Yaml.Exception -> Exception
extractException rawException =
    Exception (Yaml.situationRef rawException) (map extractWord (Yaml.words rawException))

-- TODO
extractExceptions :: [Yaml.Exception] -> [Exception]
extractExceptions = map extractException

extractWord :: Yaml.Word -> WordInfo
extractWord rawWord = 
    WordInfo (Yaml.label rawWord) (Word (Yaml.word rawWord)) (Word (Yaml.translation rawWord))

extractWordSet :: [Yaml.Word] -> [WordInfo]
extractWordSet = map extractWord

extractExample :: Yaml.WordInfo -> Example
extractExample rawWordInfo = 
    Example (extractWordSet $ Yaml.wordInfo rawWordInfo) (Yaml.ruleRef rawWordInfo) (extractExceptions $ Yaml.exceptions rawWordInfo)

extractExamples :: Yaml.Group -> [Example]
extractExamples rawGroup =
    map extractExample $ Yaml.wordsInfo rawGroup

extractWordInfo :: Yaml.Group -> ((String, String), [Example])
extractWordInfo rawGroup = 
    ((Yaml.sectionRef rawGroup, Yaml.conceptRef rawGroup), extractExamples rawGroup)

extractExampleDict :: [Yaml.Group] -> ExampleDict
extractExampleDict =
    foldr (uncurry insertToValueList . extractWordInfo) Map.empty

updateWordList :: ExampleDict -> Concept -> Concept
updateWordList exampleDict conceptInput =
    conceptInput { examples = Map.findWithDefault [] (concept conceptInput, section conceptInput) exampleDict }

updateBookWithExamples :: ExampleDict -> Book -> Book
updateBookWithExamples exampleDict =
    map (updateWordList exampleDict)

addExamples :: Yaml.Examples -> Book -> Book
addExamples rawExamples =
    updateBookWithExamples (extractExampleDict rawExamples)

buildBook :: Yaml.Book -> Yaml.Examples -> Book
buildBook rawBook rawExample =
    addExamples rawExample (concatMap buildSection rawBook)