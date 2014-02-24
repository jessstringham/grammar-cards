with module TestHelper where



checkEither :: (a -> String) -> Either String a -> String -> String 
checkEither _ (Left err) _ = err
checkEither function (Right dataThing) expected = show (function dataThing == expected)
