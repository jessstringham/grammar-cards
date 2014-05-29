module LanguageCenter.Util.Helper
( splitList
, insertToValueList
) where

import qualified Data.Map as Map (Map, insert, lookup)

splitList :: [(a, [b])] -> ([a], [b])
splitList list =
    (\x -> (fst x, concat $ snd x)) (unzip list)

insertToValueList :: (Ord a) => a -> [b] -> Map.Map a [b] -> Map.Map a [b]
insertToValueList key value mapList =
    case item of
        (Nothing) -> Map.insert key value mapList
        (Just i)  -> Map.insert key (value ++ i) mapList
  where item = Map.lookup key mapList
