



--data Template = Template { raw_rule :: String
--                       , apply_rule :: Maybe 

data Expr = Word String 
          | PlaceHolder String Bool
          | StringModifier RemoveThis AddThis
          deriving (Show)

data RemoveThis = RemoveThis [OptionalChar] deriving (Show)
data AddThis = AddThis String deriving (Show)

data OptionalChar = OptionalChar Char Bool deriving (Show)

type RawTemplate = String

parseRuleString = parseSentenceString

parseSentenceString :: RawTemplate -> [Expr]
parseSentenceString text = parseSentenceString' text []


maybeAttachWord word rest =
    if word == "" then 
        rest
    else
        (Word $ reverse word):rest

parseSentenceString' :: RawTemplate -> String -> [Expr]
parseSentenceString' [] [] = []
parseSentenceString' [] word = maybeAttachWord word []
parseSentenceString' (first:rest) word
    | first == '|' = 
        let (expressions, remaining) = getStringModifier rest in
            maybeAttachWord word (expressions:(parseSentenceString remaining))
    | first == '<' =
        let (expressions, remaining) = getPlaceHolder rest in
            maybeAttachWord word (expressions:(parseSentenceString remaining))
    | otherwise =
        parseSentenceString' rest (first:word)



getPlaceHolder :: RawTemplate -> (Expr, RawTemplate)
getPlaceHolder whole@(letter:rest)
    | letter == '_' = (getPlaceHolder' rest [] True)
    | otherwise = (getPlaceHolder' whole [] False)

getPlaceHolder' :: RawTemplate -> String -> Bool -> (Expr, RawTemplate)
getPlaceHolder' (letter:rest) parsedText translation
    | letter == '>' =
        (PlaceHolder (reverse parsedText) translation, rest)
    | otherwise =
        getPlaceHolder' rest (letter:parsedText) translation


getStringModifier :: RawTemplate -> (Expr, RawTemplate)
getStringModifier rest = getRemoveRule rest []

getRemoveRule :: RawTemplate -> [OptionalChar] -> (Expr, RawTemplate)
getRemoveRule (letter:next:rest) parsedText
    | letter == '|' = 
        let (addRule, remaining) = getAddRule (next:rest) in
            ((StringModifier (RemoveThis $ reverse parsedText) addRule), remaining)
    | next == '?' = 
        let parsedTextProgress = (OptionalChar letter True):parsedText in
            getRemoveRule rest parsedTextProgress
    | otherwise =
        let parsedTextProgress = (OptionalChar letter False):parsedText in
            getRemoveRule (next:rest) parsedTextProgress

getAddRule :: RawTemplate -> (AddThis, RawTemplate)
getAddRule text = getAddRule' text ""

getAddRule' :: RawTemplate -> String -> (AddThis, RawTemplate)
getAddRule' (letter:rest) parsedTextProgress 
    | letter == '|' =
        ((AddThis $ reverse parsedTextProgress), rest)
    | otherwise =
        getAddRule' rest (letter:parsedTextProgress)






