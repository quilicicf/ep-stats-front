module Optionize exposing (optionize)

import Html exposing (..)
import Html.Attributes exposing (..)

import Msg exposing (..)

optionize : String -> List String -> List ( Html Msg )
optionize selectedValue values =
  List.map ( optionize_ selectedValue ) values

optionize_ : String -> String -> Html Msg
optionize_ selectedValue value_ =
  option
    [ selected ( value_ == selectedValue ), value value_ ]
    [ text value_ ]
