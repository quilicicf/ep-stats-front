module GetAt exposing (getAt)

getAt : Int -> List a -> Maybe a
getAt index list = List.drop index list |> List.head
