module MapWithPreviousAndNext exposing (mapWithPreviousAndNext)

import List exposing (..)

import GetAt exposing (..)

linkElement : List a -> Int -> a -> (Maybe a, a, Maybe a)
linkElement list index element =
  ( ( getAt ( index - 1 ) list )
  ,  element
  ,  ( getAt ( index + 1 ) list )
  )

mapWithPreviousAndNext : ( (Maybe a, a, Maybe a) -> b) -> List a -> List b
mapWithPreviousAndNext mapper list =
  indexedMap ( linkElement list ) list |> map mapper
