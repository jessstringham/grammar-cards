import System.IO  

import DataStructures
--import System.Environment



test :: IO ()
test = do
    book_location:_ <- getArgs
    yamlData <- BS.readFile book_location
    let stuff = Data.Yaml.decodeEither yamlData :: Either String Book