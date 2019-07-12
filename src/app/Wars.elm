module Wars exposing (warBonuses, sanitizeExternalWarBonus)

sanitizeExternalWarBonus : String -> String
sanitizeExternalWarBonus externalWarBonus =
  List.filter ((==) externalWarBonus) warBonuses
    |> List.head
    |> Maybe.withDefault defaultWarBonus

defaultWarBonus : String
defaultWarBonus = "UNKNOWN_BONUS"

warBonuses : List String
warBonuses =
  [ "HEAL"
  , "ATTACK"
  , "ARROWS"
  , defaultWarBonus
  ]
