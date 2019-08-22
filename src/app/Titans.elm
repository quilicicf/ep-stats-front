module Titans exposing (DetailedColor, titanColors, titanColorFromString, allTitanColors)

import Translations exposing (Translations)

type alias DetailedColor =
  { code : String -- The code (to compare with the gsheet)
  , nameGetter : Translations -> String -- The displayable name, i.e. RED
  , icon: String -- The CSS class for the appropriate icon
  }

equals : String -> DetailedColor -> Bool
equals colorAsString titanColor = colorAsString == titanColor.code

titanColorFromString : String -> DetailedColor
titanColorFromString colorAsString = List.filter ( equals colorAsString ) titanColors
  |> List.head
  |> Maybe.withDefault allTitanColors

allTitanColors : DetailedColor
allTitanColors = DetailedColor "ALL" .all "fas fa-question"

titanColors : List DetailedColor
titanColors =
  [ allTitanColors
  , DetailedColor "RED"    .red    "fas fa-fire"
  , DetailedColor "GREEN"  .green  "fas fa-seedling"
  , DetailedColor "BLUE"   .blue   "fas fa-snowflake"
  , DetailedColor "HOLY"   .holy   "fas fa-sun"
  , DetailedColor "DARK"   .dark   "fas fa-skull"
  ]


