module TakeLast exposing (takeLast)

takeLast : Int -> List a -> List a
takeLast period list = List.reverse list |> List.take period |> List.reverse
