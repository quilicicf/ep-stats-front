module Flip exposing (flip)

flip : (a -> b -> c) -> b -> a -> c
flip method b a = method a b
