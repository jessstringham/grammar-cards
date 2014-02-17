
import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import qualified YamlBook as Yaml
import System.Environment
import YamlParsers
import Book
import qualified Data.Map as Map
import CardGenerator

splitList :: [(a, [b])] -> ([a], [b])
splitList list =
    (\x -> (fst x, concat $ snd x)) (unzip list)

insertToValueList :: (Ord a) => a -> [b] -> Map.Map a [b] -> Map.Map a [b]
insertToValueList key value map =
    let item = Map.lookup key map in
    case item of
        (Nothing) -> Map.insert key value map
        (Just i)  -> Map.insert key (value ++ i) map

extractRule :: String -> Yaml.Rule -> Rule
extractRule situationName rawRule = 
    Rule (Yaml.rule rawRule) situationName (Template (Yaml.back rawRule))

extractSituationRule :: Yaml.Situation -> (Situation, [Rule])
extractSituationRule rawSituation =
    let sitname = Yaml.situation rawSituation in
    let frontCard = Yaml.front rawSituation in
    let rules = map (extractRule sitname) (Yaml.rules rawSituation) in

    ((Situation sitname (Template frontCard)), rules)

extractSituationsRules :: Yaml.Concept -> ([Situation], [Rule])
extractSituationsRules rawConcept =
    let situations = Yaml.situations rawConcept in
    let situationList = map extractSituationRule situations in
    splitList situationList

extractRequiredWords :: Yaml.Concept -> [String]
extractRequiredWords rawConcept =
    Yaml.wordlist rawConcept

buildConcept :: String -> Yaml.Concept -> Concept
buildConcept sectionName rawConcept =
    let conceptName = Yaml.concept rawConcept in
    let (situations, rules) = extractSituationsRules rawConcept in
    let requiredWords = extractRequiredWords rawConcept in

    emptyConcept { section=sectionName
                 , concept=conceptName
                 , requiredWords=requiredWords
                 , situations=situations
                 , rules=rules}

buildSection :: Yaml.Section -> [Concept]
buildSection rawSection =
    let buildThisConcept = buildConcept $ Yaml.section rawSection in
    map buildThisConcept (Yaml.concepts rawSection)

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
extractWordSet rawWords =
    map extractWord rawWords

extractExample :: Yaml.WordInfo -> Example
extractExample rawWordInfo = 
    Example (extractWordSet $ Yaml.wordInfo rawWordInfo) (Yaml.ruleRef rawWordInfo) (extractExceptions $ Yaml.exceptions rawWordInfo)

extractExamples :: Yaml.Group -> [Example]
extractExamples rawGroup =
    map extractExample $ Yaml.wordsInfo rawGroup

extractWordInfo :: Yaml.Group -> ((String, String), [Example])
extractWordInfo rawGroup = 
    (((Yaml.sectionRef rawGroup), (Yaml.conceptRef rawGroup)), extractExamples rawGroup)



extractExampleDict :: [Yaml.Group] -> ExampleDict
extractExampleDict rawWordInfos =
    foldr (\y -> ((\s -> insertToValueList (fst s) (snd s)) (extractWordInfo y))) Map.empty rawWordInfos


updateWordList :: ExampleDict -> Concept -> Concept
updateWordList exampleDict conceptInput =
    conceptInput { examples = Map.findWithDefault [] (concept conceptInput, section conceptInput) exampleDict }


updateBookWithExamples :: ExampleDict -> Book -> Book
updateBookWithExamples exampleDict book =
    map (updateWordList exampleDict) book


addExamples :: Yaml.Examples -> Book -> Book
addExamples rawExamples book =
    updateBookWithExamples (extractExampleDict rawExamples) book

buildBook :: Yaml.Book -> Yaml.Examples -> Book
buildBook rawBook rawExample =
    addExamples rawExample (concat $ map buildSection rawBook)




checkEither :: Either [Char] t -> t
checkEither (Left err) = error err
checkEither (Right dataThing) = dataThing

main :: IO ()
main = do
    bookLocation:wordsLocation:_ <- getArgs

    yamlData <- BS.readFile bookLocation
    let book_data = Data.Yaml.decodeEither yamlData :: Either String Yaml.Book
    let bookYaml = checkEither book_data
    --putStrLn (show bookYaml)
    
    yamlData <- BS.readFile wordsLocation
    let wordsData = Data.Yaml.decodeEither yamlData :: Either String Yaml.Examples
    let wordsYaml = checkEither wordsData
    --putStrLn $ show wordsYaml

    putStrLn $ show $ getAllCards $ buildBook bookYaml wordsYaml

