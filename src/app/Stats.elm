module Stats exposing (StatsExtender,
  fetchAllStats,
  updateStats, viewStats,
  defaultFilterPeriod
  )

import Dict exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

import Maybe exposing (withDefault)
import MaybeExtra exposing (hasValue)

import String.Interpolate exposing (interpolate)

import Url exposing (..)

import Msg exposing (..)
import CustomStyle exposing (customStyle)
import CreateQueryString exposing (createQueryString)
import TitanStats exposing (TitanStats, updateTitanStats, viewMaybeTitanStats)
import WarStats exposing (WarStats, updateWarStats)
import GenericStatsFilter exposing (GenericStatsFilterExtender, viewGenericFilterForm)

------------
-- MODELS --
------------

type alias StatsExtender r =
  { r
  | filteredMember: String
  , filteredPeriod: Int
  , titanStats: Maybe TitanStats
  , warStats: Maybe WarStats
  }

defaultFilterPeriod : Int
defaultFilterPeriod = 30

-----------
-- UTILS --
-----------

batchGetQueryString : List ( String, String )
batchGetQueryString = [
    ( "ranges", "Titans!A:AAZ" ),
    ( "ranges", "Wars!A:AAZ" ),
    ( "valueRenderOption", "UNFORMATTED_VALUE" )
  ]

computeSheetDataUrl : String -> String
computeSheetDataUrl sheetId =
  let
    url : Url
    url = Url
      Url.Https
      "sheets.googleapis.com"
      Nothing -- Port
      (interpolate "/v4/spreadsheets/{0}/values:batchGet" [ sheetId ] ) -- Path
      ( Just ( createQueryString batchGetQueryString ) ) -- Query
      Nothing -- Fragment
  in
    Url.toString url

createBearerHeader : String -> Http.Header
createBearerHeader accessToken = Http.header "Authorization" ( interpolate "Bearer {0}" [ accessToken ] )

fetchAllStats : String -> String -> Cmd Msg
fetchAllStats sheetId accessToken = Http.request
  { method = "GET"
  , headers = [ createBearerHeader accessToken ]
  , url = computeSheetDataUrl sheetId
  , body = Http.emptyBody
  , expect = Http.expectString (StatsMsg << GotStats)
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

    maybeWarStats : Maybe WarStats
    maybeWarStats = stats.warStats

    hasStats : Bool
    hasStats = hasValue maybeTitanStats && hasValue maybeWarStats

    members : List String
    members = withDefault ( TitanStats [] Dict.empty ) stats.titanStats
      |> .titanScores
      |> Dict.keys
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
    GotStats httpResult ->
      case httpResult of
        Ok statsAsString ->
          { model
          | titanStats = updateTitanStats statsAsString
          , warStats = updateWarStats statsAsString
          }
        Err _ -> model

    NewMemberSelected newSelectedMember ->
      { model | filteredMember = newSelectedMember }

    NewPeriodSelected newPeriodAsString ->
      { model | filteredPeriod = withDefault defaultFilterPeriod (String.toInt newPeriodAsString) }

