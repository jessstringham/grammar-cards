import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import System.Environment

import LanguageCenter.Util.Helper
import LanguageCenter.Reader.BookFromYaml
import qualified LanguageCenter.Reader.YamlBook as Yaml
import LanguageCenter.Processor.CardGenerator
import LanguageCenter.Util.PrettyPrint

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

    --print $ ppBook $ buildBook bookYaml wordsYaml

    mapM_ (print . show . ppCard) (getAllCards $ buildBook bookYaml wordsYaml)

    --print (buildBook bookYaml wordsYaml)
