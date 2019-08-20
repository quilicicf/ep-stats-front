module Wars exposing (WarBonus, warBonuses, warBonusFromString, allWarBonuses)

type alias WarBonus =
  { name : String
  , icon : String
  }

equals : String -> WarBonus -> Bool
equals warBonusAsString warBonus = warBonusAsString == warBonus.name

warBonusFromString : String -> WarBonus
warBonusFromString warBonusAsString =List.filter ( equals warBonusAsString ) warBonuses
 |> List.head
 |> Maybe.withDefault allWarBonuses


allWarBonuses : WarBonus
allWarBonuses = { name = "ALL", icon = "fas fa-question" }

warBonuses : List WarBonus
warBonuses =
  [ allWarBonuses
  , WarBonus "HEAL"    "fas fa-heartbeat"
  , WarBonus "ATTACK"  "fas fa-arrow-up"
  , WarBonus "ARROWS"  "fas fa-bullseye"
  ]
