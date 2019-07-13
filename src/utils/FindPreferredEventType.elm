module FindPreferredEventType exposing (findPreferredEventType)

import Dict exposing (..)

import Hits exposing (Hits, computeHitsAverage)

addIfExisting : Maybe Int -> Maybe Hits -> Maybe Hits
addIfExisting maybeValueToAdd maybeHits =
  case maybeValueToAdd of
    Just valueToAdd ->
      case maybeHits of
        Just hits ->
          Just
            { totalDamage = hits.totalDamage + valueToAdd
            , hitsNumber = hits.hitsNumber + 1
            }
        Nothing -> Just { totalDamage = valueToAdd, hitsNumber = 1 }
    Nothing -> maybeHits

accumulator : ( s -> String ) -> ( s -> Maybe Int ) -> s -> Dict String Hits -> Dict String Hits
accumulator rawEventTypeExtractor damageExtractor damageContainer seed =
  let
    rawEventType : String
    rawEventType = rawEventTypeExtractor damageContainer

    maybeDamage : Maybe Int
    maybeDamage = damageExtractor damageContainer
  in
    Dict.update rawEventType ( addIfExisting maybeDamage ) seed

findPreferredEventType : ( s -> String ) -> ( s -> Maybe Int ) -> List s -> Maybe String
findPreferredEventType rawEventTypeExtractor damageExtractor scores =
  List.foldl ( accumulator rawEventTypeExtractor damageExtractor ) Dict.empty scores
    |> Dict.map (\_ hits -> computeHitsAverage hits)
    |> Dict.toList
    |> List.sortBy Tuple.second
    |> List.reverse
    |> List.head
    |> Maybe.map Tuple.first
