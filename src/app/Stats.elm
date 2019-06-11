module Stats exposing (StatsExtender,
  fetchTitanStats, fetchWarStats,
  updateStats, viewStats
  )

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

import MaybeExtra exposing (hasValue)

import String.Interpolate exposing (interpolate)

import Msg exposing (..)
import TitanStats exposing (..)
import GenericStatsFilter exposing (GenericStatsFilter)

------------
-- MODELS --
------------

type alias StatsExtender r =
  { r
  | genericStatsFilter: GenericStatsFilter
  , titanStats: Maybe TitanStats
  , warStats: Maybe String
  }

-----------
-- UTILS --
-----------

computeSheetUrl : String -> String -> String -> String
computeSheetUrl range sheetId apiKey = interpolate
  "https://sheets.googleapis.com/v4/spreadsheets/{0}/values/{1}?key={2}" [ sheetId, range, apiKey ]

fetchTitanStats : String -> String -> Cmd Msg
fetchTitanStats sheetId apiKey = Http.get
  { url = computeSheetUrl "Titans!A:AAZ" sheetId apiKey
  , expect = Http.expectString (StatsMsg << GotTitanStats)
  }

fetchWarStats : String -> String -> Cmd Msg
fetchWarStats sheetId apiKey = Http.get
  { url = computeSheetUrl "Wars!A%3AAZ" sheetId apiKey
  , expect = Http.expectString (StatsMsg << GotWarStats)
  }

----------
-- VIEW --
----------

viewStats : StatsExtender r -> Html Msg
viewStats stats =
  let
    maybeTitanStats : Maybe TitanStats
    maybeTitanStats = stats.titanStats

    maybeWarStats : Maybe String
    maybeWarStats = stats.warStats

    hasStats : Bool
    hasStats = hasValue maybeTitanStats && hasValue maybeWarStats
  in
    if hasStats then
      case maybeTitanStats of
        Just titanStats -> viewTitanStats stats.genericStatsFilter titanStats
        Nothing ->
          div [ class "" ] [
            span [] [ text "Data received yay!" ]
          ]
    else
      div [ class "" ] [
          span [] [ text "Spinner yay!" ]
        ]

------------
-- UPDATE --
------------

updateStats : StatsMsg -> StatsExtender r -> StatsExtender r
updateStats msg model =
  case msg of
    GotTitanStats httpResult ->
      case httpResult of
        Ok titanStatsAsString ->
          { model | titanStats = updateTitanStats titanStatsAsString }
        Err _ -> model
    GotWarStats httpResult ->
      case httpResult of
        Ok warStatsAsString ->
          { model | warStats = Just warStatsAsString }
        Err _ -> model
