module SafeParseInt exposing (safeParseInt)

import ParseInt exposing (parseInt)

safeParseInt : String -> Maybe Int
safeParseInt intAsString =
  case parseInt intAsString of
    Ok int -> Just int
    Err _ -> Nothing
