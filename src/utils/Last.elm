module Last exposing (last)

last : List a -> Maybe a
last = List.reverse >> List.head
