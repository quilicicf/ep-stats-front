module TitanStats exposing (
  TitanStats, MemberTitanScore, TitanColor,
  updateTitanStats, viewMaybeTitanStats
  )

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

type alias TitanStats =
  { dates : List String
  , titanScores : Dict String ( List MemberTitanScore )
  }

type alias RawStats = { valueRanges : List RawSheet }

type alias RawSheet = { values: List ( List String ) }

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

rawSheetStatsDecoder : Decoder RawSheet
rawSheetStatsDecoder =
  Decode.succeed RawSheet
   |> required "values" ( Decode.list (Decode.list string) )

rawStatsDecoder : Decoder RawStats
rawStatsDecoder =
  Decode.succeed RawStats
    |> required "valueRanges" ( Decode.list rawSheetStatsDecoder )

decodeRawStats : String -> RawStats
decodeRawStats statsAsString =
  let
    decodingResult : Result Decode.Error RawStats
    decodingResult = Decode.decodeString rawStatsDecoder statsAsString

  in
    case decodingResult of
      Ok rawStats -> rawStats
      Err _ -> RawStats []

decodeTitanStats : String -> TitanStats
decodeTitanStats statsAsString =
  let
    rawStats : RawStats
    rawStats = decodeRawStats statsAsString

    rawTitanStats : RawSheet
    rawTitanStats = withDefault ( RawSheet [] ) ( getAt 0 rawStats.valueRanges )

    dates : List String
    dates = extractDates rawTitanStats

    scores : Dict String ( List MemberTitanScore )
    scores = extractTitanScores rawTitanStats
  in
    TitanStats dates scores

extractDates : RawSheet -> List String
extractDates rawTitanStats =
  List.drop 1 rawTitanStats.values
    |> List.map List.head
    |> List.map ( withDefault "???" )

extractTitanScores : RawSheet -> Dict String ( List MemberTitanScore )
extractTitanScores rawTitanStats =
  let
    headerRow : List String
    headerRow = withDefault [] ( List.head rawTitanStats.values )

    data : List ( List String )
    data = List.drop 1 rawTitanStats.values

    allianceResults : ( String, List MemberTitanScore )
    allianceResults = extractTitanDataForMember data 0 fixedIndexes.totalIndex allianceName

    membersResults : List ( String, List MemberTitanScore )
    membersResults = List.drop fixedIndexes.number headerRow
          |> List.indexedMap ( extractTitanDataForMember data fixedIndexes.number )

  in
    Dict.fromList ( allianceResults :: membersResults )

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

extractTitanDataForMember : ( List ( List String ) ) -> Int -> Int -> String -> ( String, List MemberTitanScore )
extractTitanDataForMember data offset index memberPseudo =
  let
    memberDataIndex : Int
    memberDataIndex = offset + index

    memberScores : List MemberTitanScore
    memberScores = List.map ( extractMemberTitanScore memberDataIndex ) data

  in
    ( memberPseudo, memberScores )

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
updateTitanStats statsAsString = Just ( decodeTitanStats statsAsString )

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
    titanScoresElements = Dict.map ( viewTitanMemberScores genericStatsFilter .score ) titanStats.titanScores
      |> Dict.values

    titanValuesElements : List (Html Msg)
    titanValuesElements = Dict.map ( viewTitanMemberScores genericStatsFilter .value ) titanStats.titanScores
      |> Dict.values

  in
    div [ class "graphs-container" ] [
      div [ class "chart-title" ] [ text "Titan scores" ],
      div [ class "graph-container" ] [
        table [ class "chart", customStyle [ ("--titans", titansNumberAsString) ] ] [
          thead [] [ tr [] ( dateRowHeadingElement :: titanDatesElements ) ],
          tbody [] titanScoresElements
        ]
      ],
      div [ class "chart-title" ] [ text "Member's team-relative value" ],
      div [ class "graph-container" ] [
        table [ class "chart", customStyle [ ("--titans", titansNumberAsString) ] ] [
          thead [] [ tr [] ( dateRowHeadingElement :: titanDatesElements ) ],
          tbody [] titanValuesElements
        ]
      ]
    ]

viewTitanMemberScores : GenericStatsFilterExtender r  -> ( MemberTitanScore -> Maybe Int ) -> String -> List MemberTitanScore -> Html Msg
viewTitanMemberScores genericStatsFilter scoreExtractor memberPseudo memberTitanScores =
  let
    isSelected : Bool
    isSelected = genericStatsFilter.filteredMember == memberPseudo

    titanScoresRowHeading : Html Msg
    titanScoresRowHeading = th [ class "labels" ] [ text genericStatsFilter.filteredMember ]

    filteredValues : List MemberTitanScore
    filteredValues = List.reverse memberTitanScores
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
      hidden (not isSelected),
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
