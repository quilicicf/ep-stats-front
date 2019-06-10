module GraphUtils exposing (getLineStartX, getLineEndX, getLineStartY, getLineEndY)

getLineStartX : Maybe Int -> String
getLineStartX maybePreviousScore =
  case maybePreviousScore of
    Just _ -> "0%"
    Nothing -> "50%"

getLineStartY : Maybe Int -> String
getLineStartY maybePreviousScore =
  case maybePreviousScore of
    Just previousScore -> String.fromInt previousScore
    Nothing -> "var(--value)"

getLineEndX : Maybe Int -> String
getLineEndX maybeNextScore =
  case maybeNextScore of
    Just _ -> "100%"
    Nothing -> "50%"

getLineEndY : Maybe Int -> String
getLineEndY maybeNextScore =
  case maybeNextScore of
    Just nextScore -> String.fromInt nextScore
    Nothing -> "var(--value)"
