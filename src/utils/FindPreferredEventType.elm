module FindPreferredEventType exposing (findPreferredEventType)

import Dict exposing (..)

import Hits exposing (Hits, computeHitsAverage)

addIfExisting : Int -> Maybe Hits -> Maybe Hits
addIfExisting valueToAdd maybeHits =
  case maybeHits of
    Just hits ->
      Just
        { totalDamage = hits.totalDamage + valueToAdd
        , hitsNumber = hits.hitsNumber + 1
        }
    Nothing -> Just { totalDamage = valueToAdd, hitsNumber = 1 }

accumulator : ( s -> String ) -> ( s -> Int ) -> s -> Dict String Hits -> Dict String Hits
accumulator rawEventTypeExtractor damageExtractor damageContainer seed =
  let
    rawEventType : String
    rawEventType = rawEventTypeExtractor damageContainer

    maybeDamage : Int
    maybeDamage = damageExtractor damageContainer
  in
    Dict.update rawEventType ( addIfExisting maybeDamage ) seed

findPreferredEventType : ( s -> String ) -> ( s -> Int ) -> List s -> Maybe String
findPreferredEventType rawEventTypeExtractor damageExtractor scores =
  List.foldl ( accumulator rawEventTypeExtractor damageExtractor ) Dict.empty scores
    |> Dict.map (\_ hits -> computeHitsAverage hits)
    |> Dict.toList
    |> List.sortBy Tuple.second
    |> List.reverse
    |> List.head
    |> Maybe.map Tuple.first
