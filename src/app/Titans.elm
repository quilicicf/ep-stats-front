module Titans exposing (DetailedColor, titanColors, titanColorFromString, allTitanColors)

type alias DetailedColor =
  { name : String -- THe displayable name, i.e. RED
  , code: String -- The CSS custom property, i.e. var(--red)
  }

equals : String -> DetailedColor -> Bool
equals colorAsString titanColor = colorAsString == titanColor.name

titanColorFromString : String -> DetailedColor
titanColorFromString colorAsString = List.filter ( equals colorAsString ) titanColors
  |> List.head
  |> Maybe.withDefault allTitanColors

allTitanColors : DetailedColor
allTitanColors = DetailedColor "ALL" "var(--black)"

titanColors : List DetailedColor
titanColors =
  [ allTitanColors
  , DetailedColor "RED" "var(--red)"
  , DetailedColor "GREEN" "var(--green)"
  , DetailedColor "BLUE" "var(--blue)"
  , DetailedColor "HOLY" "var(--holy)"
  , DetailedColor "DARK" "var(--dark)"
  ]


