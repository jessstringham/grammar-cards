
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
    book_location:_ <- getArgs
    yamlData <- BS.readFile book_location
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Book
    let book = checkEither stuff
    putStrLn $ show book