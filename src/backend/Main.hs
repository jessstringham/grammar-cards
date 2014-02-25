import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import System.Environment

import BookFromYaml
import CardGenerator
import Helper
import qualified YamlBook as Yaml


main :: IO ()
main = do
    bookLocation:wordsLocation:_ <- getArgs

    bookRawYaml <- BS.readFile bookLocation
    let book_data = Data.Yaml.decodeEither bookRawYaml :: Either String Yaml.Book
    let bookYaml = checkEither book_data
    --putStrLn (show bookYaml)
    
    wordsRawYaml <- BS.readFile wordsLocation
    let wordsData = Data.Yaml.decodeEither wordsRawYaml :: Either String Yaml.Examples
    let wordsYaml = checkEither wordsData
    --putStrLn $ show wordsYaml

    print (getAllCards $ buildBook bookYaml wordsYaml)

