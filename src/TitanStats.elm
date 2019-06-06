module TitanStats exposing (..)

import Debug exposing (log)

import Json.Decode as Decode exposing(Value, Decoder, string)
import Json.Decode.Pipeline exposing (required)

import List.Extra exposing (getAt)

import Maybe exposing (withDefault)

import ParseInt exposing (parseInt)

import Regex exposing (..)

import MaybeExtra exposing (hasValue)

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

nonMemberRows : Int
nonMemberRows = 6

colorIndex : Int
colorIndex = 4

starIndex : Int
starIndex = 3

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
      Ok rawTitanStats -> rawTitanStats
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

  in
    List.drop nonMemberRows headerRow
      |> List.indexedMap ( extractTitanDataForMember data )

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

extractTitanDataForMember : ( List ( List String ) ) -> Int -> String -> MemberTitanScores
extractTitanDataForMember data index memberPseudo =
  let
    rawValues : List String
    rawValues = List.map ( getAt ( nonMemberRows + index ) ) data
      |> List.map ( withDefault "N/A" )

    values : List ( Maybe Int )
    values  = List.map safeParseInt rawValues

    maxScore : Maybe Int
    maxScore = List.filter hasValue values
      |> List.map (withDefault 0)
      |> List.maximum

    memberScores : List MemberTitanScore
    memberScores = List.indexedMap ( extractMemberTitanScore index data ) data

  in
    MemberTitanScores memberPseudo maxScore False memberScores

extractMemberTitanScore : Int -> List ( List String ) -> Int -> List String -> MemberTitanScore
extractMemberTitanScore memberIndex data titanIndex row =
  let
    value : Maybe Int
    value = getAt ( nonMemberRows + memberIndex ) row
      |> withDefault ""
      |> safeParseInt

    previousValue : Maybe Int
    previousValue = getAt ( titanIndex - 1 ) data
      |> withDefault []
      |> getAt ( nonMemberRows + memberIndex )
      |> withDefault ""
      |> safeParseInt

    nextValue : Maybe Int
    nextValue = getAt ( titanIndex + 1 ) data
      |> withDefault []
      |> getAt ( nonMemberRows + memberIndex )
      |> withDefault ""
      |> safeParseInt

    titanColor : TitanColor
    titanColor = getAt colorIndex row
      |> withDefault ""
      |> decodeTitanColor

    titanStars : Int
    titanStars = getAt starIndex row
      |> withDefault ""
      |> safeParseInt
      |> withDefault 0
  in
    MemberTitanScore value titanColor titanStars previousValue nextValue
