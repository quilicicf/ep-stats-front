module Titans exposing (DetailedColor, titanColors, titanColorFromString, allTitanColors)

type alias DetailedColor =
  { name : String -- THe displayable name, i.e. RED
  , icon: String -- The CSS class for the appropriate icon
  }

equals : String -> DetailedColor -> Bool
equals colorAsString titanColor = colorAsString == titanColor.name

titanColorFromString : String -> DetailedColor
titanColorFromString colorAsString = List.filter ( equals colorAsString ) titanColors
  |> List.head
  |> Maybe.withDefault allTitanColors

allTitanColors : DetailedColor
allTitanColors = DetailedColor "ALL" "fas fa-question"

titanColors : List DetailedColor
titanColors =
  [ allTitanColors
  , DetailedColor "RED"   "fas fa-fire"
  , DetailedColor "GREEN" "fas fa-seedling"
  , DetailedColor "BLUE"  "fas fa-snowflake"
  , DetailedColor "HOLY"  "fas fa-sun"
  , DetailedColor "DARK"  "fas fa-skull"
  ]


