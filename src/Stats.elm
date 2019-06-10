module Stats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

import Maybe exposing (withDefault, map)
import MaybeExtra exposing (hasValue)

import String.Interpolate exposing (interpolate)

import CustomStyle exposing (customStyle)
import Msg exposing (..)
import TitanStats exposing (..)

------------
-- MODELS --
------------

type alias StatsExtender r =
  { r
  | titanStats: Maybe TitanStats
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
        Just titanStats -> viewTitanStats titanStats
        Nothing ->
          div [ class "" ] [
            span [] [ text "Data received yay!" ]
          ]
    else
      div [ class "" ] [
          span [] [ text "Spinner yay!" ]
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
      table [ class "chart", customStyle [ ("--titans", titansNumberAsString) ] ] [
        caption [] [ text "A table that shows user performance on titans" ],
        thead [] [
          tr [] titanDates
        ],
        tbody [] titanScores
      ]
    ]

valueAsString : String -> String
valueAsString value = interpolate "'{0}'" [ value ]

viewTitanMemberScores : MemberTitanScores -> Html Msg
viewTitanMemberScores memberTitanScores =
  let
    htmlClass : String
    htmlClass = if memberTitanScores.isSelected then "selected-member" else "hidden-member"

    maxValue : String
    maxValue = withDefault 0 memberTitanScores.max |> String.fromInt

    rowHeading : Html Msg
    rowHeading = th [ class "labels" ] [ text memberTitanScores.pseudo ]

    row : List (Html Msg)
    row = rowHeading :: List.map viewTitanMemberScore memberTitanScores.scores

  in
    tr [
      class htmlClass,
      customStyle [
        ("--max", maxValue),
        ("--max-as-string", valueAsString maxValue)
      ]
    ] row

getLineStartX : Maybe Int -> String
getLineStartX maybePreviousScore =
  case maybePreviousScore of
    Just _ -> "0%"
    Nothing -> "50%"

getLineStartY : Maybe Int -> String
getLineStartY maybePreviousScore =
  case maybePreviousScore of
    Just previousScore -> String.fromInt previousScore
    Nothing -> "var(--value)"

getLineEndX : Maybe Int -> String
getLineEndX maybeNextScore =
  case maybeNextScore of
    Just _ -> "100%"
    Nothing -> "50%"

getLineEndY : Maybe Int -> String
getLineEndY maybeNextScore =
  case maybeNextScore of
    Just nextScore -> String.fromInt nextScore
    Nothing -> "var(--value)"

viewTitanMemberScore : MemberTitanScore -> Html Msg
viewTitanMemberScore memberTitanScore =
  let
    value : String
    value = memberTitanScore.value
      |> withDefault 0
      |> String.fromInt

    detailedTitanColor : DetailedColor
    detailedTitanColor = detailTitanColor memberTitanScore.titanColor

  in
    td
      [ class "chart-value"
      , customStyle
        [ ("--value", value)
        ,("--value-as-string", valueAsString value)
        ,("--titan-color", valueAsString detailedTitanColor.name)
        ,("--titan-color-code", detailedTitanColor.code)
        ,("--titan-stars-as-string", valueAsString (String.fromInt memberTitanScore.titanStars))
        ,("--line-start-x", getLineStartX memberTitanScore.previousScore)
        ,("--line-start-y", getLineStartY memberTitanScore.previousScore)
        ,("--line-end-x", getLineEndX memberTitanScore.nextScore)
        ,("--line-end-y", getLineEndY memberTitanScore.nextScore)
        ]
      ] [
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
          { model | titanStats = updateTitanStats titanStatsAsString }
        Err _ -> model
    GotWarStats httpResult ->
      case httpResult of
        Ok warStatsAsString ->
          { model | warStats = Just warStatsAsString }
        Err _ -> model

updateTitanStats : String -> Maybe TitanStats
updateTitanStats titanStatsAsString =
  let
    titanStats : TitanStats
    titanStats = decodeTitanStats titanStatsAsString

  in
    Just titanStats
