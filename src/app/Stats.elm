module Stats exposing (StatsExtender,
  fetchTitanStats, fetchWarStats,
  updateStats, viewStats
  )

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

import Maybe exposing (withDefault)
import MaybeExtra exposing (hasValue)

import String.Interpolate exposing (interpolate)

import Msg exposing (..)
import TitanStats exposing (TitanStats, updateTitanStats, viewMaybeTitanStats)
import GenericStatsFilter exposing (GenericStatsFilterExtender, viewGenericFilterForm)

------------
-- MODELS --
------------

type alias StatsExtender r =
  { r
  | filteredMember: String
  , filteredPeriod: Int
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

    members : List String
    members = withDefault (TitanStats [] []) stats.titanStats
      |> .titanScores
      |> List.map .pseudo
  in
    if hasStats then
      div [ class "stats" ] [
        viewGenericFilterForm stats members,
        viewMaybeTitanStats stats maybeTitanStats
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

    NewMemberSelected newSelectedMember ->
      { model | filteredMember = newSelectedMember }
