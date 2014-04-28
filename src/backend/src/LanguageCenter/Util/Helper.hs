module LanguageCenter.Util.Helper
( splitList
, insertToValueList
, checkEither
, combinations
) where

import qualified Data.Map as Map

splitList :: [(a, [b])] -> ([a], [b])
splitList list =
    (\x -> (fst x, concat $ snd x)) (unzip list)

insertToValueList :: (Ord a) => a -> [b] -> Map.Map a [b] -> Map.Map a [b]
insertToValueList key value mapList =
    case item of
        (Nothing) -> Map.insert key value mapList
        (Just i)  -> Map.insert key (value ++ i) mapList
  where item = Map.lookup key mapList

checkEither :: Either String t -> t
checkEither (Left err) = error err
checkEither (Right dataThing) = dataThing

combinations :: [a] -> [b] -> [(a, b)]
combinations [] _ = []
combinations _ [] = []
combinations (a1:a_rest) b@(b1:b_rest) = (a1, b1):(combinations [a1] b_rest ++ combinations a_rest b)
