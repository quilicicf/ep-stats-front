module MaybeExtra exposing (hasValue, maybeify)

hasValue : Maybe a -> Bool
hasValue maybe = case maybe of
  Just _ -> True
  Nothing -> False

maybeify : String -> Maybe String
maybeify string = case string of
  "" -> Nothing
  _ -> Just string
