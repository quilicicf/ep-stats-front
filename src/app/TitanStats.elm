module TitanStats exposing (TitanStats, updateTitanStats, viewMaybeTitanStats)

import Debug exposing (log)

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
import AllianceName exposing (allianceName)
import GenericStatsFilter exposing (GenericStatsFilter)
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
    allianceResults = extractTitanDataForMember data 0 fixedIndexes.totalIndex allianceName

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

    memberScores : List MemberTitanScore
    memberScores = List.indexedMap ( extractMemberTitanScore memberDataIndex data ) data

  in
    MemberTitanScores memberPseudo memberScores

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

viewMaybeTitanStats : GenericStatsFilter -> Maybe TitanStats -> Html Msg
viewMaybeTitanStats genericStatsFilter maybeTitanStats =
  case maybeTitanStats of
    Just titanStats -> wrapTitanStats [ viewTitanStats genericStatsFilter titanStats ]
    Nothing -> wrapTitanStats []

wrapTitanStats : List ( Html Msg ) -> Html Msg
wrapTitanStats titanStats = div [ class "titan-stats" ] titanStats

viewTitanStats : GenericStatsFilter -> TitanStats -> Html Msg
viewTitanStats genericStatsFilter titanStats =
  let
    titanDatesElements : List (Html Msg)
    titanDatesElements = "Titan date" :: titanStats.dates
      |> List.reverse
      |> List.take genericStatsFilter.period
      |> List.reverse
      |> List.map ( \date -> th [] [ text date ] )

    titansNumberAsString : String
    titansNumberAsString = List.length titanDatesElements
      |> String.fromInt

    titanScoresElements : List (Html Msg)
    titanScoresElements = List.map ( viewTitanMemberScores genericStatsFilter ) titanStats.titanScores

  in
    div [ class "graph-container" ] [
      table [ class "chart", customStyle [ ("--titans", titansNumberAsString) ] ] [
        caption [] [ text "A table that shows user performance on titans" ],
        thead [] [
          tr [] titanDatesElements
        ],
        tbody [] titanScoresElements
      ]
    ]

viewTitanMemberScores : GenericStatsFilter  -> MemberTitanScores -> Html Msg
viewTitanMemberScores genericStatsFilter memberTitanScores =
  let
    htmlClass : String
    htmlClass = if genericStatsFilter.user == memberTitanScores.pseudo then "selected-member" else "hidden-member"

    filteredValues : List MemberTitanScore
    filteredValues = List.reverse memberTitanScores.scores
      |> List.take genericStatsFilter.period
      |> List.reverse

    maxValue : String
    maxValue = List.map .value filteredValues
      |> List.filter hasValue
      |> List.map ( withDefault 0 )
      |> List.maximum
      |> withDefault 0
      |> String.fromInt

    rowElements : List (Html Msg)
    rowElements = List.indexedMap ( viewTitanMemberScore genericStatsFilter.period ) filteredValues

  in
    tr [
      class htmlClass,
      customStyle [
        ("--max", maxValue),
        ("--max-as-string", valueAsString maxValue)
      ]
    ] rowElements

viewTitanMemberScore : Int -> Int -> MemberTitanScore -> Html Msg
viewTitanMemberScore itemsNumber index memberTitanScore =
  let
    value : String
    value = memberTitanScore.value
      |> withDefault 0
      |> String.fromInt

    previousScore : Maybe Int
    previousScore = if index == 0 then Nothing else memberTitanScore.previousScore

    nextScore : Maybe Int
    nextScore = if index == itemsNumber - 1 then Nothing else memberTitanScore.nextScore

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
        ,("--line-start-x", getLineStartX previousScore)
        ,("--line-start-y", getLineStartY previousScore)
        ,("--line-end-x", getLineEndX nextScore)
        ,("--line-end-y", getLineEndY nextScore)
        ]
      ] [
        span [class "score-presenter"] [ text value ]
      ]
