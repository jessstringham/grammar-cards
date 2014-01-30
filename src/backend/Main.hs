import System.IO  

import LanguageLibrary
import IOHelper
import Control.Monad.State
import System.Environment

optionsForBook :: OptionList GrammarBook
optionsForBook = [("That is all, thanks", Just),("Delete", \x -> Nothing)]

type BookMonad v = StateT Database IO v


main :: IO ()
main = do
    databaseFile:_ <- getArgs
    let saveDatabaseFunction = saveDatabase databaseFile

    -- load the database
    db <- loadDatabase databaseFile

    -- do stuff with the database
    putStrLn $ showDatabase db

    -- get grammar book
    putStrLn "What book do you want to open?"
    bookName <- getLine
    let (book, dbWithBook) = openOrAddNewGrammarBook bookName db
    putStrLn "Great, what would you like to do today?"

    function <- getFunctionFromUser optionsForBook

    -- Apply the function to the selected book
    let newBook = function book

    -- if the book is Nothing, delete it. otherwise, udpate it!
    let dbWithUpdatedBook = updateOrDeleteGrammarBook bookName newBook dbWithBook

    -- save teh database
    saveDatabaseFunction dbWithUpdatedBook

