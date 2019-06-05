module Stats exposing (..)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

import Maybe exposing (withDefault, map)

import String.Interpolate exposing (interpolate)

import Msg exposing (..)
import TitanStats exposing (..)

------------
-- MODELS --
------------

type alias Stats =
  { war : Maybe String
  , titan : Maybe TitanStats
  }

type alias StatsExtender r =
  { r
  | stats: Maybe Stats
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

viewStats : Maybe Stats -> Html Msg
viewStats maybeStats = case maybeStats of
  Nothing ->
    div [ class "" ] [
        span [] [ text "Spinner yay!" ]
      ]

  Just stats ->
    let
      maybeTitanStats : Maybe TitanStats
      maybeTitanStats = stats.titan
    in
      case maybeTitanStats of
        Just titanStats -> viewTitanStats titanStats
        Nothing ->
          div [ class "" ] [
            span [] [ text "Data received yay!" ]
          ]

viewTitanStats : TitanStats -> Html Msg
viewTitanStats titanStats =
  let
    titansNumberAsString : String
    titansNumberAsString = List.length titanStats.titanScores |> String.fromInt

    titanDates : List (Html Msg)
    titanDates = "Titan date" :: titanStats.dates
      |> List.map ( \date -> th [] [ text date ] )

    titanScores : List (Html Msg)
    titanScores = List.map ( viewTitanMemberScores ) titanStats.titanScores

  in
    div [ class "graph-container" ] [
      table [ class "chart", style "--titans" titansNumberAsString ] [
        caption [] [ text "A table that shows user performance on titans" ],
        thead [] [
          tr [] titanDates
        ],
        tbody [] titanScores
      ]
    ]

viewTitanMemberScores : MemberTitanScores -> Html Msg
viewTitanMemberScores memberTitanScores =
  let
    htmlClass : String
    htmlClass = if memberTitanScores.isSelected then "selected-member" else "hidden-member"

    maxValue : String
    maxValue = withDefault 0 memberTitanScores.max |> String.fromInt

    maxValueAsString : String
    maxValueAsString = interpolate "'{0}'" [ maxValue ]

    rowHeading : Html Msg
    rowHeading = th [ class "labels" ] [ text memberTitanScores.pseudo ]

    row : List (Html Msg)
    row = rowHeading :: List.map viewTitanMemberScore memberTitanScores.scores

  in
    tr [ class htmlClass, style "--max" maxValue, style "--max-as-string" maxValueAsString ] row

viewTitanMemberScore : MemberTitanScore -> Html Msg
viewTitanMemberScore memberTitanScore =
  let
    value : String
    value = memberTitanScore.value
      |> withDefault 0
      |> String.fromInt

  in
    td [ class "chart-value" ] [
      span [class "score-presenter"] [ text value ]
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
          let
            newTitanStats : Maybe TitanStats
            newTitanStats = updateTitanStats titanStatsAsString

            newWarStats : Maybe String
            newWarStats = Nothing

            newStats : Stats
            newStats = Stats newWarStats newTitanStats

          in
            { model | stats = Just newStats }
        Err _ -> model
    GotWarStats httpResult ->
      case httpResult of
        Ok warStatsAsString ->
          let
            newTitanStats : Maybe TitanStats
            newTitanStats = map ( \stats -> withDefault ( TitanStats [] [] ) stats.titan ) model.stats

            newWarStats : Maybe String
            newWarStats = Just warStatsAsString

            newStats : Stats
            newStats = Stats newWarStats newTitanStats

          in
            { model | stats = Just newStats }
        Err _ -> model

updateTitanStats : String -> Maybe TitanStats
updateTitanStats titanStatsAsString =
  let
    titanStats : TitanStats
    titanStats = decodeTitanStats titanStatsAsString

  in
    Just titanStats
