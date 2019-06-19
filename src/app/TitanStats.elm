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
import ComputeValue exposing (computeValue)
import MapWithPreviousAndNext exposing (mapWithPreviousAndNext)
import GenericStatsFilter exposing (GenericStatsFilterExtender)
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
  { score : Maybe Int
  , value: Maybe Int
  , titanColor : TitanColor
  , titanStars : Int
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
  , membersIndex = 5
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
    scores = extractTitanScoresList rawTitanStats
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
    memberScores = List.map ( extractMemberTitanScore memberDataIndex ) data

  in
    MemberTitanScores memberPseudo memberScores

extractMemberTitanScore : Int -> List String -> MemberTitanScore
extractMemberTitanScore memberIndex row =
  let
    score : Maybe Int
    score = getAt ( memberIndex ) row
      |> withDefault ""
      |> safeParseInt

    allianceScore : Int
    allianceScore = getAt fixedIndexes.totalIndex row
      |> withDefault "0"
      |> safeParseInt
      |> withDefault 0

    membersNumber : Int
    membersNumber = getAt fixedIndexes.membersIndex row
      |> withDefault "0"
      |> safeParseInt
      |> withDefault 0

    value : Maybe Int
    value = Maybe.map ( computeValue allianceScore membersNumber ) score

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
    MemberTitanScore score value titanColor titanStars

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

viewMaybeTitanStats : GenericStatsFilterExtender r -> Maybe TitanStats -> Html Msg
viewMaybeTitanStats genericStatsFilter maybeTitanStats =
  case maybeTitanStats of
    Just titanStats -> wrapTitanStats [ viewTitanStats genericStatsFilter titanStats ]
    Nothing -> wrapTitanStats []

wrapTitanStats : List ( Html Msg ) -> Html Msg
wrapTitanStats titanStats = div [ class "titan-stats" ] titanStats

viewTitanStats : GenericStatsFilterExtender r -> TitanStats -> Html Msg
viewTitanStats genericStatsFilter titanStats =
  let
    dateRowHeadingElement : Html Msg
    dateRowHeadingElement = th [] [ text "Titan date" ]

    titanDatesElements : List (Html Msg)
    titanDatesElements = titanStats.dates
      |> List.reverse
      |> List.take genericStatsFilter.filteredPeriod
      |> List.reverse
      |> List.map ( \date -> th [] [ text date ] )

    titansNumberAsString : String
    titansNumberAsString = List.length titanDatesElements
      |> String.fromInt

    titanScoresElements : List (Html Msg)
    titanScoresElements = List.map ( viewTitanMemberScores genericStatsFilter .score ) titanStats.titanScores

    titanValuesElements : List (Html Msg)
    titanValuesElements = List.map ( viewTitanMemberScores genericStatsFilter .value ) titanStats.titanScores

  in
    div [ class "graphs-container" ] [
      div [ class "graph-container" ] [
        table [ class "chart", customStyle [ ("--titans", titansNumberAsString) ] ] [
          caption [] [ text "A table that shows user performance on titans" ],
          thead [] [ tr [] ( dateRowHeadingElement :: titanDatesElements ) ],
          tbody [] titanScoresElements
        ]
      ],
      div [ class "graph-container" ] [
        table [ class "chart", customStyle [ ("--titans", titansNumberAsString) ] ] [
          caption [] [ text "A table that shows user performance on titans" ],
          thead [] [ tr [] ( dateRowHeadingElement :: titanDatesElements ) ],
          tbody [] titanValuesElements
        ]
      ]
    ]

viewTitanMemberScores : GenericStatsFilterExtender r  -> ( MemberTitanScore -> Maybe Int ) -> MemberTitanScores -> Html Msg
viewTitanMemberScores genericStatsFilter scoreExtractor memberTitanScores =
  let
    htmlClass : String
    htmlClass = if genericStatsFilter.filteredMember == memberTitanScores.pseudo then "selected-member" else "hidden-member"

    titanScoresRowHeading : Html Msg
    titanScoresRowHeading = th [ class "labels" ] [ text genericStatsFilter.filteredMember ]

    filteredValues : List MemberTitanScore
    filteredValues = List.reverse memberTitanScores.scores
      |> List.take genericStatsFilter.filteredPeriod
      |> List.reverse

    maxScore : String
    maxScore = List.map scoreExtractor filteredValues
      |> List.filter hasValue
      |> List.map ( withDefault 0 )
      |> List.maximum
      |> withDefault 0
      |> String.fromInt

    rowElements : List (Html Msg)
    rowElements = mapWithPreviousAndNext ( viewTitanMemberScore scoreExtractor ) filteredValues

  in
    tr [
      class htmlClass,
      customStyle [
        ("--max", maxScore),
        ("--max-as-string", valueAsString maxScore)
      ]
    ] ( titanScoresRowHeading :: rowElements )

viewTitanMemberScore : ( MemberTitanScore -> Maybe Int )
  -> (Maybe MemberTitanScore, MemberTitanScore, Maybe MemberTitanScore)
  -> Html Msg
viewTitanMemberScore  scoreExtractor (maybePreviousMemberTitanScore, memberTitanScore, maybeNextMemberTitanScore) =
  let
    score : String
    score = scoreExtractor memberTitanScore
      |> withDefault 0
      |> String.fromInt

    previousScore : Maybe Int
    previousScore = Maybe.andThen scoreExtractor maybePreviousMemberTitanScore

    nextScore : Maybe Int
    nextScore = Maybe.andThen scoreExtractor maybeNextMemberTitanScore

    detailedTitanColor : DetailedColor
    detailedTitanColor = detailTitanColor memberTitanScore.titanColor

  in
    td
      [ class "chart-value"
      , customStyle
        [ ("--value", score)
        , ("--value-as-string", valueAsString score)
        , ("--titan-color", valueAsString detailedTitanColor.name)
        , ("--titan-color-code", detailedTitanColor.code)
        , ("--titan-stars-as-string", valueAsString (String.fromInt memberTitanScore.titanStars))
        , ("--line-start-x", getLineStartX previousScore)
        , ("--line-start-y", getLineStartY previousScore)
        , ("--line-end-x", getLineEndX nextScore)
        , ("--line-end-y", getLineEndY nextScore)
        ]
      ] [
        span [class "score-presenter"] [ text score ]
      ]
