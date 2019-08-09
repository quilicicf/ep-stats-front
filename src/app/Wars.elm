module Wars exposing (warBonuses, warBonusFromString, sanitizeExternalWarBonus)

warBonusFromString : String -> Maybe String
warBonusFromString externalWarBonus =
  List.filter ((==) externalWarBonus) warBonuses
   |> List.head

sanitizeExternalWarBonus : String -> String
sanitizeExternalWarBonus externalWarBonus = warBonusFromString externalWarBonus |> Maybe.withDefault defaultWarBonus

defaultWarBonus : String
defaultWarBonus = "UNKNOWN_BONUS"

warBonuses : List String
warBonuses =
  [ "HEAL"
  , "ATTACK"
  , "ARROWS"
  , defaultWarBonus
  ]
