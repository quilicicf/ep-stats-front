module Titans exposing (TitanColor(..), TitanColorData, titanColorsByCode, titanColorFromString, getTitanColorData)

import AssocList as Dict exposing (..)

import Translations exposing (..)

type alias TitanColorData =
  { code : String -- The code (to compare with the gsheet)
  , name : String -- The displayable name, i.e. RED
  , icon: String -- The CSS class for the appropriate icon
  }

type TitanColor = RED | GREEN | BLUE | HOLY | DARK | ALL

titanColorFromString : String -> TitanColor
titanColorFromString colorAsString = Dict.get colorAsString titanColorsByCode
  |> Maybe.withDefault ALL

getTitanColorData : Translations -> TitanColor -> TitanColorData
getTitanColorData translations titanColor = case titanColor of
  RED   -> TitanColorData "RED"   translations.red   "fas fa-fire"
  GREEN -> TitanColorData "GREEN" translations.green "fas fa-seedling"
  BLUE  -> TitanColorData "BLUE"  translations.blue  "fas fa-snowflake"
  HOLY  -> TitanColorData "HOLY"  translations.holy  "fas fa-sun"
  DARK  -> TitanColorData "DARK"  translations.dark  "fas fa-skull"
  ALL   -> TitanColorData "ALL"   translations.all   "fas fa-question"

titanColorsByCode : Dict String TitanColor
titanColorsByCode = Dict.fromList [
    ( "RED", RED ),
    ( "GREEN", GREEN ),
    ( "BLUE", BLUE ),
    ( "HOLY", HOLY ),
    ( "DARK", DARK ),
    ( "ALL", ALL )
  ]
