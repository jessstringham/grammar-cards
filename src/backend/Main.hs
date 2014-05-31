import qualified Data.ByteString.Char8 as BS (readFile)
import Data.Yaml (decodeEither)
import System.Environment (getArgs)

import LanguageCenter.Reader.BookFromYaml (buildBook)
import qualified LanguageCenter.Reader.YamlBook as Yaml (Book, Examples)
import LanguageCenter.Processor.CardGenerator (getAllCards)
import LanguageCenter.Util.PrettyPrint (ppCard, ppBook)

uneither = either error id

main :: IO ()
main = do
    bookLocation:wordsLocation:_ <- getArgs

    bookRawYaml <- BS.readFile bookLocation
    let book_data = uneither $ decodeEither bookRawYaml :: Yaml.Book
    --putStrLn (show book_data)

    wordsRawYaml <- BS.readFile wordsLocation
    let words_data = uneither $ decodeEither wordsRawYaml :: Yaml.Examples
    --putStrLn $ show words_data

    --print $ ppBook $ buildBook bookYaml words_data

    mapM_ (print . ppCard) (getAllCards $ buildBook book_data words_data)

    --print (buildBook bookYaml wordsYaml)
