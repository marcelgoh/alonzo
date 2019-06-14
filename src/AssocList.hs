-- Basic operations on association lists

module AssocList where

searchKeys :: Eq k => [(k, v)] -> k -> Maybe v
searchKeys [] _ = Nothing
searchKeys ((k, v) : rest) query =
  if k == query then Just v else searchKeys rest query

searchValues :: Eq v => [k] -> [(k, v)] -> v -> [k]
searchValues acc [] _ = acc
searchValues acc ((k, v) : rest) query =
  if v == query then
    searchValues (k : acc) rest query
  else
    searchValues acc rest query

merge :: [(k, v)] -> [(k, v)] -> [(k, v)]
merge reversed forward =
  case reversed of
    [] -> forward
    (r : rs) -> merge rs (r : forward)

replacePair :: Eq k => Eq v => [(k, v)] -> k -> v -> [(k, v)]
replacePair assocList key value =
  let
    iterateKeys reversed [] = Nothing
    iterateKeys reversed ((k, v) : rest) =
      if k == key then
        Just $ merge reversed $ (key, value) : rest
      else
        iterateKeys ((k, v) : reversed) rest
  in
  case iterateKeys [] assocList of
    Just l -> l
    Nothing -> (key, value) : assocList
