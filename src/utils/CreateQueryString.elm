module CreateQueryString exposing (createQueryString)

import Url exposing (percentEncode)

createQueryParameter : ( String, String ) -> String
createQueryParameter ( name, value ) =
  ( percentEncode name ) ++ "=" ++ ( percentEncode value )

createQueryString : List ( String, String ) -> String
createQueryString queryAsTupleList = List.map createQueryParameter queryAsTupleList |> String.join "&"
