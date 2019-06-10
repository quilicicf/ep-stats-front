module CustomStyle exposing (customStyle)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)

import String.Interpolate exposing (interpolate)

singleStyle : ( String, String ) -> String
singleStyle ( propertyName, propertyValue) =
  interpolate "{0}: {1}" [ propertyName, propertyValue ]

customStyle : List ( String, String ) -> Attribute msg
customStyle styles =
    let
      style : String
      style = List.map singleStyle styles
        |> String.join "; "
    in
      attribute "style" style
