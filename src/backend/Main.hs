
import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import Definitions
import System.Environment
import YamlParsers

checkEither :: Either [Char] t -> t
checkEither (Left err)= error err
checkEither (Right dataThing) = dataThing

main :: IO ()
main = do
    bookLocation:wordsLocation:_ <- getArgs

    yamlData <- BS.readFile bookLocation
    let book_data = Data.Yaml.decodeEither yamlData :: Either String BookYaml
    let book = checkEither book_data
    putStrLn (show book)
    
    yamlData <- BS.readFile wordsLocation
    let wordsData = Data.Yaml.decodeEither yamlData :: Either String ExamplesYaml
    let words = checkEither wordsData
    putStrLn $ show words