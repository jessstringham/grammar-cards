
import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import DataStructures
import System.Environment


--checkEither :: Eq a => Either String a -> a -> String
checkEither (Left err)= error err
checkEither (Right dataThing) = dataThing

test :: IO ()
test = do
    book_location:_ <- getArgs
    yamlData <- BS.readFile book_location
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Book
    let book = checkEither stuff
    putStrLn $ show book