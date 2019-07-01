module MaybeExtra exposing (hasValue)

hasValue : Maybe a -> Bool
hasValue maybe = case maybe of
  Just _ -> True
  Nothing -> False
