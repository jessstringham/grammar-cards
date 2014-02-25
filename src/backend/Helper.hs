module Helper
( splitList
, insertToValueList
, checkEither
) where

import qualified Data.Map as Map

splitList :: [(a, [b])] -> ([a], [b])
splitList list =
    (\x -> (fst x, concat $ snd x)) (unzip list)

insertToValueList :: (Ord a) => a -> [b] -> Map.Map a [b] -> Map.Map a [b]
insertToValueList key value map =
    case item of
        (Nothing) -> Map.insert key value map
        (Just i)  -> Map.insert key (value ++ i) map
  where item = Map.lookup key map

checkEither :: Either String t -> t
checkEither (Left err) = error err
checkEither (Right dataThing) = dataThing