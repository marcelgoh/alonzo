-- Basic operations on association lists

module AssocList where

searchKeys :: Eq k => [(k, v)] -> k -> Maybe v
searchKeys [] _ = Nothing
searchKeys ((k, v) : rest) query =
  if k == query then Just v else searchKeys rest query

searchValues :: Eq v => [(k, v)] -> v -> Maybe k
searchValues [] _ = Nothing
searchValues ((k, v) : rest) query =
  if v == query then Just k else searchValues rest query

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
    iterateValues reversed [] = Nothing
    iterateValues reversed ((k, v) : rest) =
      if v == value then
        Just $ merge reversed $ (key, value) : rest
      else
        iterateValues ((k, v) : reversed) rest
  in
  case iterateKeys [] assocList of
    Just l -> l
    Nothing ->
      case iterateValues [] assocList of
        Just l -> l
        Nothing -> (key, value) : assocList
