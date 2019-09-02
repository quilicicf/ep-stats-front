module CustomStyle exposing (customStyle)

import Html exposing (..)
import Html.Attributes exposing (..)

singleStyle : ( String, String ) -> String
singleStyle ( propertyName, propertyValue) = String.join "" [ propertyName, ": ", propertyValue ]

customStyle : List ( String, String ) -> Attribute msg
customStyle styles =
    let
      style : String
      style = List.map singleStyle styles
        |> String.join "; "
    in
      attribute "style" style
