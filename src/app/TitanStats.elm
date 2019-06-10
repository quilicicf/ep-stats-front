module TitanStats exposing (TitanStats, updateTitanStats, viewTitanStats)

import Debug exposing (log)

import Dict exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Json.Decode as Decode exposing(Value, Decoder, string)
import Json.Decode.Pipeline exposing (required)

import List.Extra exposing (getAt)

import Maybe exposing (withDefault)

import ParseInt exposing (parseInt)

import Regex exposing (..)

import Msg exposing (Msg)
import CustomStyle exposing (customStyle)
import ValueAsString exposing (valueAsString)
import MaybeExtra exposing (hasValue)
import GraphUtils exposing (getLineStartX, getLineEndX, getLineStartY, getLineEndY)

------------
-- MODELS --
------------

type TitanColor = Red | Green | Blue | Holy | Dark | Unknown

type alias DetailedColor =
  { name : String -- THe displayable name, i.e. RED
  , code: String -- The CSS custom property, i.e. var(--red)
  }

type alias MemberTitanScore =
  { value : Maybe Int
  , titanColor : TitanColor
  , titanStars : Int
  , previousScore: Maybe Int
  , nextScore: Maybe Int
  }

type alias MemberTitanScores =
  { pseudo : String
  , max : Maybe Int
  , isSelected : Bool
  , scores: List MemberTitanScore
  }

type alias TitanStats =
  { dates : List String
  , titanScores : List ( MemberTitanScores )
  }

type alias RawTitanStats = { values: List ( List String ) }

-----------
-- UTILS --
-----------

fixedIndexes =
  { number = 6
  , dateIndex = 0
  , totalIndex = 1
  , lifeIndex = 2
  , starIndex = 3
  , colorIndex = 4
  , membersIndex = 4
  }

detailTitanColor : TitanColor -> DetailedColor
detailTitanColor titanColor =
  case titanColor of
    Red -> DetailedColor "RED" "var(--red)"
    Green -> DetailedColor "GREEN" "var(--green)"
    Blue -> DetailedColor "BLUE" "var(--blue)"
    Holy -> DetailedColor "HOLY" "var(--holy)"
    Dark -> DetailedColor "DARK" "var(--dark)"
    Unknown -> DetailedColor "UNKNOWN" "var(--black)"

decodeTitanColor : String -> TitanColor
decodeTitanColor colorAsString =
  case colorAsString of
    "RED" -> Red
    "GREEN" -> Green
    "BLUE" -> Blue
    "HOLY" -> Holy
    "DARK" -> Dark
    _ -> Unknown

rawTitanStatsDecoder : Decoder RawTitanStats
rawTitanStatsDecoder =
  Decode.succeed RawTitanStats
    |> required "values" ( Decode.list (Decode.list string) )

decodeRawTitanStats : String -> RawTitanStats
decodeRawTitanStats titanStatsAsString =
  let
    decodingResult : Result Decode.Error RawTitanStats
    decodingResult = Decode.decodeString rawTitanStatsDecoder titanStatsAsString
  in
    case decodingResult of
      Ok rawTitanStats ->
        { rawTitanStats | values = rawTitanStats.values }

      Err _ -> RawTitanStats []

decodeTitanStats : String -> TitanStats
decodeTitanStats titanStatsAsString =
  let
    rawTitanStats : RawTitanStats
    rawTitanStats = decodeRawTitanStats titanStatsAsString

    dates : List String
    dates = extractDates rawTitanStats

    scores : List MemberTitanScores
    scores = log "Scores" (extractTitanScoresList rawTitanStats)
  in
    TitanStats dates scores

extractDates : RawTitanStats -> List String
extractDates rawTitanStats =
  List.drop 1 rawTitanStats.values
    |> List.map List.head
    |> List.map ( withDefault "???" )

extractTitanScoresList : RawTitanStats -> List MemberTitanScores
extractTitanScoresList rawTitanStats =
  let
    headerRow : List String
    headerRow = withDefault [] ( List.head rawTitanStats.values )

    data : List ( List String )
    data = List.drop 1 rawTitanStats.values

    allianceResults : MemberTitanScores
    allianceResults = extractTitanDataForMember data 0 fixedIndexes.totalIndex "Alliance"

    membersResults : List MemberTitanScores
    membersResults = List.drop fixedIndexes.number headerRow
          |> List.indexedMap ( extractTitanDataForMember data fixedIndexes.number )

  in
    log "Toto" allianceResults :: membersResults

noWhiteSpaceRegex : Regex
noWhiteSpaceRegex = withDefault Regex.never ( Regex.fromString "\\s+" )

safeParseInt : String -> Maybe Int
safeParseInt intAsString =
  let
    safeIntAsString : String
    safeIntAsString = Regex.replace noWhiteSpaceRegex (\_ -> "") intAsString
  in
    case parseInt safeIntAsString of
      Ok int -> Just int
      Err _ -> Nothing

extractTitanDataForMember : ( List ( List String ) ) -> Int -> Int -> String -> MemberTitanScores
extractTitanDataForMember data offset index memberPseudo =
  let
    memberDataIndex : Int
    memberDataIndex = offset + index

    rawValues : List String
    rawValues = List.map ( getAt memberDataIndex ) data
      |> List.map ( withDefault "N/A" )

    values : List ( Maybe Int )
    values  = List.map safeParseInt rawValues

    maxScore : Maybe Int
    maxScore = List.filter hasValue values
      |> List.map (withDefault 0)
      |> List.maximum

    memberScores : List MemberTitanScore
    memberScores = List.indexedMap ( extractMemberTitanScore memberDataIndex data ) data

  in
    MemberTitanScores memberPseudo maxScore ( index == 0 ) memberScores

toIntOrNothing : Maybe String -> Maybe Int
toIntOrNothing maybeIntAsString =
  case maybeIntAsString of
    Just intAsString -> safeParseInt intAsString
    Nothing -> Nothing

extractMemberTitanScore : Int -> List ( List String ) -> Int -> List String -> MemberTitanScore
extractMemberTitanScore memberIndex data titanIndex row =
  let
    value : Maybe Int
    value = getAt ( memberIndex ) row
      |> withDefault ""
      |> safeParseInt

    previousValue : Maybe Int
    previousValue = getAt ( titanIndex - 1 ) data
      |> withDefault []
      |> getAt ( memberIndex )
      |> toIntOrNothing

    nextValue : Maybe Int
    nextValue = getAt ( titanIndex + 1 ) data
      |> withDefault []
      |> getAt ( memberIndex )
      |> toIntOrNothing

    titanColor : TitanColor
    titanColor = getAt fixedIndexes.colorIndex row
      |> withDefault ""
      |> decodeTitanColor

    titanStars : Int
    titanStars = getAt fixedIndexes.starIndex row
      |> withDefault ""
      |> safeParseInt
      |> withDefault 0
  in
    MemberTitanScore value titanColor titanStars previousValue nextValue

------------
-- UPDATE --
------------

updateTitanStats : String -> Maybe TitanStats
updateTitanStats titanStatsAsString =
  let
    titanStats : TitanStats
    titanStats = decodeTitanStats titanStatsAsString

  in
    Just titanStats

----------
-- VIEW --
----------

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
