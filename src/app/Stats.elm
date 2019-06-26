module Stats exposing (StatsExtender,
  fetchAllStats,
  updateStats, viewStats
  )

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

import Maybe exposing (withDefault)
import MaybeExtra exposing (hasValue)

import String.Interpolate exposing (interpolate)

import Msg exposing (..)
import CustomStyle exposing (customStyle)
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

computeSheetUrl : String -> String -> String
computeSheetUrl range sheetId = interpolate
  "https://sheets.googleapis.com/v4/spreadsheets/{0}/values/{1}" [ sheetId, range ]

createBearerHeader : String -> Http.Header
createBearerHeader accessToken = Http.header "Authorization" ( interpolate "Bearer {0}" [ accessToken ] )

fetchAllStats : String -> String -> Cmd Msg
fetchAllStats sheetId accessToken = Cmd.batch [ fetchTitanStats sheetId accessToken, fetchWarStats sheetId accessToken ]

fetchTitanStats : String -> String -> Cmd Msg
fetchTitanStats sheetId accessToken = Http.request
  { method = "GET"
  , headers = [ createBearerHeader accessToken ]
  , url = computeSheetUrl "Titans!A:AAZ" sheetId
  , body = Http.emptyBody
  , expect = Http.expectString (StatsMsg << GotTitanStats)
  , timeout = Nothing
  , tracker = Nothing
  }

fetchWarStats : String -> String -> Cmd Msg
fetchWarStats sheetId accessToken = Http.request
  { method = "GET"
  , headers = [ createBearerHeader accessToken ]
  , url = computeSheetUrl "Wars!A%3AAZ" sheetId
  , body = Http.emptyBody
  , expect = Http.expectString (StatsMsg << GotWarStats)
  , timeout = Nothing
  , tracker = Nothing
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
      div [ class "stats-spinner" ] [
        div [ class "spinner-container" ] [
          span [ class "spinner-text" ] [ text "Fetching the data" ],
          div [ class "spinner" ] [
            div [ class "bar", customStyle [ ("--bar-index", "0"), ("--bar-color", "var(--red") ] ] [],
            div [ class "bar", customStyle [ ("--bar-index", "1"), ("--bar-color", "var(--green") ] ] [],
            div [ class "bar", customStyle [ ("--bar-index", "2"), ("--bar-color", "var(--blue") ] ] [],
            div [ class "bar", customStyle [ ("--bar-index", "3"), ("--bar-color", "var(--holy") ] ] [],
            div [ class "bar", customStyle [ ("--bar-index", "4"), ("--bar-color", "var(--dark") ] ] []
          ]
        ]
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
