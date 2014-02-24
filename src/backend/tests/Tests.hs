
import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import DataStructures


checkEither :: Eq a => Either String a -> a -> String
checkEither (Left err) _ = err
checkEither (Right dataThing) expected = show $ dataThing == expected


test :: IO ()
test = do

    yamlData <- BS.readFile "../../data/mocks/rule.yaml"
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Rule
    let result = checkEither stuff Rule{rule="TestRule",back="template"}
    putStrLn $ "Rules: " ++ result


    yamlData <- BS.readFile "../../data/mocks/situation.yaml"
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Situation

    let exampleSituation = Situation {
        situation = "Indefinite Article", 
        front = "(a <_noun>)", 
        rules = [
            Rule {
                rule = "en", 
                back = "en <noun>"
            } , Rule {
                rule = "ett", 
                back = "ett <noun>"
            }
        ]
    }

    -- putStrLn $ show stuff
    let result = checkEither stuff exampleSituation
    putStrLn $ "Situation: " ++ result



    yamlData <- BS.readFile "../../data/mocks/concept.yaml"
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Concept

    let exampleConcept = Concept { concept = "Nouns"
                                 , wordlist = ["noun"]
                                 , conceptTrait = []
                                 , situations = [exampleSituation]}

    -- putStrLn $ show stuff
    let result = checkEither stuff exampleConcept

    putStrLn $ "Concept: " ++ result


    yamlData <- BS.readFile "../../data/mocks/section.yaml"
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Section
    let exampleSection = Section { section = "Nouns"
                                 , concepts = [ exampleConcept ] }

    let result = checkEither stuff exampleSection

    putStrLn $ "Section: " ++ result

    yamlData <- BS.readFile "../../data/mocks/book.yaml"
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Book

    putStrLn $ show stuff

