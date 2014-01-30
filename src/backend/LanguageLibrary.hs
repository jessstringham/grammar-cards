module LanguageLibrary  
( Database
, readDatabase
, showDatabase
, openOrAddNewGrammarBook
, GrammarBook
, updateOrDeleteGrammarBook
--, createGrammarBook  
--, addNewSectionToBook  
-- , openSection  
) where 

import qualified Data.Map as Map

type Name = String

type Database = Map.Map Name GrammarBook

readDatabase :: String -> Database
readDatabase = Map.fromList . read

showDatabase :: Database -> String
showDatabase input = show $ Map.toList input

emptyDatabase :: Database
emptyDatabase = Map.empty

type GrammarBook = Map.Map Name Section

createEmptyGrammarBook :: GrammarBook
createEmptyGrammarBook = Map.empty

addGrammarBook :: Name -> GrammarBook -> Database -> Database
addGrammarBook = Map.insert

openGrammarBook :: Name -> Database -> Maybe GrammarBook
openGrammarBook = Map.lookup

deleteGrammarBook :: Name -> Database -> Database
deleteGrammarBook = Map.delete

openOrAddNewGrammarBook :: Name -> Database -> (GrammarBook, Database)
openOrAddNewGrammarBook name db = 
	let book = (openGrammarBook name db) in
		case book of 
			Nothing -> 
				let newBook = createEmptyGrammarBook in
					(newBook, addGrammarBook name newBook db)
			Just book ->
				(book, db)

updateOrDeleteGrammarBook :: Name -> Maybe GrammarBook -> Database -> Database
updateOrDeleteGrammarBook name book db = 
	case book of 
		Nothing ->
			deleteGrammarBook name db
		Just book ->
			addGrammarBook name book db

-- A grammar book contains sections for funsies

type GrammaticalConcept = String

type Section = Map.Map Name GrammaticalConcept


createEmptySection :: Section
createEmptySection = Map.empty

addSection :: Name -> Section -> GrammarBook -> GrammarBook
addSection = Map.insert

openSection :: Name -> GrammarBook -> Maybe Section
openSection = Map.lookup

deleteSection :: Name -> GrammarBook -> GrammarBook
deleteSection = Map.delete


-- addNewSectionToBook :: Name -> GrammarBook -> GrammarBook
-- addNewSectionToBook sectionName book = Map.insert sectionName [] book

-- openSection :: Name -> GrammarBook -> Maybe Section
-- openSection name grammarBook = Map.lookup name grammarBook 

