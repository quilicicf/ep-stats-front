module Stats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

import String.Interpolate exposing (interpolate)

import Msg exposing (..)

------------
-- MODELS --
------------

type alias Stats =
  { war : Maybe String
  , titan : Maybe String
  }

-----------
-- UTILS --
-----------

computeSheetUrl : String -> String -> String -> String
computeSheetUrl range sheetId apiKey = interpolate
  "https://sheets.googleapis.com/v4/spreadsheets/{0}/values/{1}?key={2}" [ sheetId, range, apiKey ]

fetchTitanStats : String -> String -> Cmd Msg
fetchTitanStats sheetId apiKey = Http.get
  { url = computeSheetUrl "Titans!A%3AAZ" sheetId apiKey
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

viewStats : Maybe Stats -> Html Msg
viewStats maybeStats = case maybeStats of
  Nothing ->
    div [ class "" ] [
        span [] [ text "Spinner yay!" ]
      ]

  Just stats ->
    div [ class "" ] [
      span [] [ text "Data received yay!" ]
    ]
