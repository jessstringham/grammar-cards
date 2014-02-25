module BookFromYaml
( buildBook
) where

import qualified Data.Map as Map

import Book
import Helper
import qualified YamlBook as Yaml

extractRule :: String -> Yaml.Rule -> Rule
extractRule situationName rawRule = 
    Rule (Yaml.rule rawRule) situationName (Template (Yaml.back rawRule))

extractSituationRule :: Yaml.Situation -> (Situation, [Rule])
extractSituationRule rawSituation =
    (Situation sitname (Template front_card), rules)
    where sitname = Yaml.situation rawSituation
          front_card = Yaml.front rawSituation
          rules = map (extractRule sitname) (Yaml.rules rawSituation)

extractSituationsRules :: Yaml.Concept -> ([Situation], [Rule])
extractSituationsRules rawConcept =
    splitList situation_list
  where situations = Yaml.situations rawConcept
        situation_list = map extractSituationRule situations

extractRequiredWords :: Yaml.Concept -> [String]
extractRequiredWords = Yaml.wordlist

buildConcept :: String -> Yaml.Concept -> Concept
buildConcept sectionName rawConcept =
    emptyConcept { section=sectionName
                 , concept=conceptName
                 , requiredWords=required_words
                 , situations=situations
                 , rules=rules}

  where conceptName = Yaml.concept rawConcept
        (situations, rules) = extractSituationsRules rawConcept
        required_words = extractRequiredWords rawConcept

buildSection :: Yaml.Section -> [Concept]
buildSection rawSection =
    map build_this_concept (Yaml.concepts rawSection)
  where build_this_concept = buildConcept $ Yaml.section rawSection

type ExampleDict = Map.Map (String, String) [Example]

extractException :: Yaml.Exception -> Exception
extractException rawException =
    Exception (Yaml.situationRef rawException) (map extractWord (Yaml.words rawException))

-- todo
extractExceptions :: [Yaml.Exception] -> [Exception]
extractExceptions rawGroup = []

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