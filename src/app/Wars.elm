module Wars exposing (WarBonus, warBonuses, warBonusFromString, allWarBonuses)

import Translations exposing (Translations)

type alias WarBonus =
  { code : String -- The code (to compare with the gsheet)
  , nameGetter : Translations -> String -- The displayable name, i.e. HEAL
  , icon : String -- The CSS class for the appropriate icon
  }

equals : String -> WarBonus -> Bool
equals warBonusAsString warBonus = warBonusAsString == warBonus.code

warBonusFromString : String -> WarBonus
warBonusFromString warBonusAsString =List.filter ( equals warBonusAsString ) warBonuses
 |> List.head
 |> Maybe.withDefault allWarBonuses


allWarBonuses : WarBonus
allWarBonuses = WarBonus "ALL" .all "fas fa-question"

warBonuses : List WarBonus
warBonuses =
  [ allWarBonuses
  , WarBonus "HEAL"    .heal    "fas fa-heartbeat"
  , WarBonus "ATTACK"  .attack  "fas fa-arrow-up"
  , WarBonus "ARROWS"  .arrows  "fas fa-bullseye"
  ]
