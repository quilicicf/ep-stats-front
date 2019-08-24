module Optionize exposing (optionizeStrings, optionizeObjects)

import Html exposing (..)
import Html.Attributes exposing (..)

optionizeStrings : String -> List String -> List ( Html msg )
optionizeStrings selectedValue allValues =
  List.map ( optionizeValue identity identity selectedValue ) allValues

optionizeObjects : ( a -> String ) -> ( a -> String ) -> a -> List a -> List ( Html msg )
optionizeObjects idExtractor labelExtractor selectedValue allValues =
  List.map ( optionizeValue idExtractor labelExtractor selectedValue ) allValues

optionizeValue : ( a -> String ) -> ( a -> String ) -> a -> a -> Html msg
optionizeValue idExtractor labelExtractor selectedValue currentValue =
  option
    [ selected ( currentValue == selectedValue ), value <| idExtractor currentValue ]
    [ text <| labelExtractor currentValue ]
