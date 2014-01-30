module IOHelper  
( loadDatabase
, saveDatabase
, OptionList
, getFunctionFromUser
) where 

import LanguageLibrary
import Control.Monad


loadDatabase :: String -> IO Database
loadDatabase databaseFile = do
    contents <- readFile databaseFile
    let db = readDatabase contents
    return (db)



-- 
{-
loadDatabase = readFile database >>= return . read
loadDatabase2 :: IO Database
loadDatabase2 = do
	liftM read $ readFile database
-}
saveDatabase :: String -> Database -> IO ()
saveDatabase file db = do
    writeFile file $ showDatabase db

type ThingToThingFunction a = a -> Maybe a

type OptionList a = [(String, ThingToThingFunction a)]


--getOptionDescriptions :: GrammarBookOptionsList -> String
getOptionDescriptions optionsList = map fst optionsList

applyLabels :: [t] -> [(Integer, t)]
applyLabels = zip [0..]

getInt :: IO Int
getInt = do
	int <- getLine
	return (read int :: Int)

getFunctionFromOption :: OptionList a -> Int -> ThingToThingFunction a
getFunctionFromOption optionsList optionID = snd $ ( optionsList !! optionID)


getFunctionFromUser :: OptionList a -> IO (ThingToThingFunction a)
getFunctionFromUser optionsList = do
    mapM print $ applyLabels $ getOptionDescriptions optionsList

    -- get a number back
    optionID <- getInt

    return $ getFunctionFromOption optionsList optionID

--openOrCreateGrammarBook :: Name -> Database -> IO ()
