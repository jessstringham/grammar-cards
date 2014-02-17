module Expr
( functionFromExpr
) where

import Template
import Book

extractWord (Word word) = word
extractWord a = error "Undefined! " ++ (show a)

lookupWordSetWord :: [WordInfo] -> String -> Bool -> String
lookupWordSetWord ws name isTranslation = 
	let wordSetWord = (head (filter (\x -> (name == (wordName x))) ws)) in
	if isTranslation then
		extractWord $ translation wordSetWord
	else
		extractWord $ word wordSetWord

-- This takes a template and returns a function that will convert a
-- wordset to a string

functionFromExpr :: [Expr] -> [WordInfo] -> String
functionFromExpr a b = functionFromExpr' a b ""

functionFromExpr' :: [Expr] -> [WordInfo] -> String -> String
functionFromExpr' [] _ accum = accum
functionFromExpr' ((PlaceHolder name bool):rest) wordset accum = 
	accum ++ (functionFromExpr' rest wordset $ lookupWordSetWord wordset name bool)
functionFromExpr' ((StringModifier removeThis addThis):rest) wordset accum = 
	functionFromExpr' rest wordset $ addCharacters addThis $ removeSomeCharacters removeThis accum
functionFromExpr' ((WordText text):rest) wordset accum = 
	functionFromExpr' rest wordset (accum ++ text)

addCharacters :: AddThis -> String -> String
addCharacters (AddThis addThis) accum =
	accum ++ addThis

removeSomeCharacters :: RemoveThis -> String -> String
removeSomeCharacters (RemoveThis removeThis) accum = removeSomeCharacters' removeThis accum

removeSomeCharacters' :: [OptionalChar] -> String -> String
removeSomeCharacters' [] accum = accum
removeSomeCharacters' (char:rest) accum = 
	removeSomeCharacters' rest (removeCharacter char accum)

removeCharacter :: OptionalChar -> String -> String
removeCharacter (OptionalChar char optional) accum = 
	if head (reverse accum) == char then
		init accum
	else
		if optional then
			accum
		else
			error "This word doesn't work!"

tests =
	let example1 = [WordInfo "noun" (Word "spelar") (Word "car")] in
	--let template1 = [PlaceHolder "noun" False, StringModifier (RemoveThis []) (AddThis "ar")] in
	let template1 = [WordText "jag "
	                , PlaceHolder "noun" False
	                , StringModifier (RemoveThis [OptionalChar 'r' True]) (AddThis "dde")] in
	functionFromExpr template1 example1

