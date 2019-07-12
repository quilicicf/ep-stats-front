module Titans exposing (DetailedColor, titanColors, titanColorFromString)

type alias DetailedColor =
  { name : String -- THe displayable name, i.e. RED
  , code: String -- The CSS custom property, i.e. var(--red)
  }

equals : String -> DetailedColor -> Bool
equals colorAsString titanColor = colorAsString == titanColor.name

titanColorFromString : String -> DetailedColor
titanColorFromString colorAsString = List.filter ( equals colorAsString ) titanColors
  |> List.head
  |> Maybe.withDefault defaultColor

defaultColor : DetailedColor
defaultColor = DetailedColor "DARK" "var(--black)"

titanColors : List DetailedColor
titanColors =
  [ DetailedColor "RED" "var(--red)"
  , DetailedColor "GREEN" "var(--green)"
  , DetailedColor "BLUE" "var(--blue)"
  , DetailedColor "GREEN" "var(--holy)"
  , DetailedColor "HOLY" "var(--dark)"
  , defaultColor
  ]


