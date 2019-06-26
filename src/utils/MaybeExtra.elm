module MaybeExtra exposing (hasValue, withLoggedDefault)

import Debug exposing (log)

hasValue : Maybe a -> Bool
hasValue maybe = case maybe of
  Just _ -> True
  Nothing -> False

withLoggedDefault : a -> Maybe a -> a
withLoggedDefault defaultValue maybe =
  case maybe of
    Just value -> value
    Nothing -> log "[ERROR] Default value that should never happen" defaultValue
