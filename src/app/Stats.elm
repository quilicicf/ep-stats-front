module Stats exposing (
  Stats, StatsExtender,
  fetchAllStats,
  updateStats, viewStats,
  defaultFilterPeriod
  )

import Dict exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http exposing (..)

import Json.Decode as Decode exposing(Value, Decoder, string)
import Json.Decode.Pipeline exposing (required)

import List.Extra exposing (getAt)

import Maybe exposing (withDefault)
import MaybeExtra exposing (hasValue)

import ParseInt exposing (parseInt)

import Regex exposing (..)

import String.Interpolate exposing (interpolate)

import Url exposing (..)

import Msg exposing (..)
import CustomStyle exposing (customStyle)
import ComputeValue exposing (computeValue)
import CreateQueryString exposing (createQueryString)
import TitanStats exposing (updateTitanStats, viewMaybeTitanStats)
import WarStats exposing (updateWarStats)
import GenericStatsFilter exposing (GenericStatsFilterExtender, viewGenericFilterForm)

------------
-- MODELS --
------------

type alias StatsExtender r =
  { r
  | filteredMember: String
  , filteredPeriod: Int
  , stats: Maybe Stats
  , filteredStats: Maybe Stats
  }

type alias MemberScore =
  { damage : Int
  , teamValue : Int
  }

type alias Stats =
  { titanDates : List String
  , warDates : List String
  , allianceTitanScores : List AllianceTitanScore
  , allianceWarScores: List AllianceWarScore
  , membersTitanScores: List MemberTitanScores
  , membersWarScores: List MemberWarScores
  }

type alias AllianceTitanScore =
  { score : Int
  , titanColor : TitanColor
  , titanStars : Int
  }

type alias AllianceWarScore =
  { score : Int
  , warBonus : WarBonus
  }

type alias MemberTitanScores =
  { pseudo : String
  , titanScores : List MemberTitanScore
  }

type alias MemberTitanScore =
  { score : Maybe MemberScore
  , titanColor : TitanColor
  , titanStars : Int
  }

type alias MemberWarScores =
  { pseudo : String
  , warScores : List MemberWarScore
  }

type alias MemberWarScore =
  { score : Maybe MemberScore
  , warBonus : WarBonus
  }

type alias FilteredStats =
  { dates : List String
  , allianceTitanScores : FilteredAllianceTitanScores
  , allianceWarScores: FilteredAllianceWarScores
  , membersTitanScores: List FilteredMemberTitanScores
  , membersWarScores: List FilteredMemberWarScores
  }

type alias FilteredAllianceTitanScores =
  { averageTitanScore : Float
  , preferredTitanColor : TitanColor
  , titanScores : List AllianceTitanScore
  }

type alias FilteredAllianceWarScores =
  { averageWarScore : Float
  , preferredWarBonus : WarBonus
  , warScores : List AllianceWarScore
  }

type alias FilteredMemberTitanScores =
  { pseudo : String
  , averageScore : Maybe MemberScore
  , preferredTitanColor : Maybe TitanColor
  , titanScores : List MemberTitanScore
  }

type alias FilteredMemberWarScores =
  { pseudo : String
  , averageScore : Maybe MemberScore
  , preferredWarBonus : Maybe WarBonus
  , warScores : List MemberWarScore
  }

type alias RawStats = { valueRanges : List RawSheet }

type alias RawSheet = { values: List ( List String ) }

-- TITANS

type TitanColor = RED | GREEN | BLUE | HOLY | DARK | UNKNOWN_COLOR

type alias DetailedColor =
  { name : String -- THe displayable name, i.e. RED
  , code: String -- The CSS custom property, i.e. var(--red)
  }

type alias TitanScore =
  { score : Int
  , teamValue : Int
  , titanColor: TitanColor
  , titanStars : Int
  }

fixedTitanIndexes =
  { number = 6
  , dateIndex = 0
  , totalIndex = 1
  , lifeIndex = 2
  , starIndex = 3
  , colorIndex = 4
  , membersIndex = 5
  }

-- WARS

type WarBonus = HEAL | ATTACK | ARROWS | UNKNOWN_BONUS

type alias WarScore =
  { score : Int
  , teamValue : Int
  , warBonus: WarBonus
  }

-----------
-- UTILS --
-----------

-- TITANS

decodeTitanColor : String -> TitanColor
decodeTitanColor colorAsString =
  case colorAsString of
    "RED" -> RED
    "GREEN" -> GREEN
    "BLUE" -> BLUE
    "HOLY" -> HOLY
    "DARK" -> DARK
    _ -> UNKNOWN_COLOR

-- WARS

decodeWarBonus : String -> WarBonus
decodeWarBonus warBonusAsString =
  case warBonusAsString of
    "HEAL" -> HEAL
    "ATTACK" -> ATTACK
    "ARROWS" -> ARROWS
    _ -> UNKNOWN_BONUS

defaultFilterPeriod : Int
defaultFilterPeriod = 30

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

----------
-- VIEW --
----------

viewStats : StatsExtender r -> Html Msg
viewStats { stats } =
  let
    hasStats : Bool
    hasStats = hasValue stats

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

--addTitanScore : Dict Int String -> Stats -> String -> Stats
--addTitanScore indexesToPseudos currentStats =
--
--
--addTitanScores : Dict Int String -> List String -> Stats -> Stats
--addTitanScores indexesToPseudos rawRow currentStats =
--  List.drop fixedIndexes.number rawRow
--    |> List.map ( addTitanScore indexesToPseudos currentStats )
--

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

computeScore : Maybe Int -> Int -> Int -> Maybe MemberScore
computeScore maybeDamage allianceScore membersNumber =
  case maybeDamage of
    Just damage -> Just ( MemberScore damage ( computeValue allianceScore membersNumber damage ) )
    Nothing -> Nothing

getMaybeIntFromRow : List String -> Int -> Maybe Int
getMaybeIntFromRow row index = getAt index row |> safeParseInt

getIntFromRow : List String -> Int -> Int -> Int
getIntFromRow row index default = getAt index row |> safeParseInt |> withDefault default

getTitanColorFromRow : List String -> Int -> TitanColor
getTitanColorFromRow row index = getAt index row |> decodeTitanColor

getWarBonusFromRow : List String -> Int -> WarBonus
getWarBonusFromRow row index = getAt index row |> decodeWarBonus

extractDates : RawSheet -> Int -> List String
extractDates rawSheet fixedIndexes =
  getAt 0 rawSheet.values
    |> withDefault []
    |> List.drop fixedIndexes

extractTitanAllianceScores : RawSheet -> List AllianceTitanScore
extractTitanAllianceScores titanSheet =
  List.drop 1 titanSheet.values
    |> List.map (\row ->
      AllianceTitanScore
        ( getIntFromRow row fixedTitanIndexes.totalIndex 0 )
        ( getTitanColorFromRow row fixedTitanIndexes.colorIndex )
        ( getIntFromRow row fixedTitanIndexes.totalIndex 0 )
      )

extractWarAllianceScores : RawSheet -> List AllianceWarScore
extractWarAllianceScores warSheet =
  List.drop 1 warSheet.values
    |> List.map (\row ->
      AllianceWarScore
        ( getIntFromRow row fixedTitanIndexes.totalIndex 0 )
        ( getWarBonusFromRow row fixedTitanIndexes.colorIndex )
      )

extractMemberTitanScore : Int -> List String -> MemberTitanScore
extractMemberTitanScore memberIndex row =
  let
    maybeDamage : Maybe Int
    maybeDamage = getMaybeIntFromRow row memberIndex

    allianceScore : Int
    allianceScore = getIntFromRow row fixedTitanIndexes.totalIndex 0

    membersNumber : Int
    membersNumber = getIntFromRow row fixedTitanIndexes.membersIndex 0

    memberScore : Maybe MemberScore
    memberScore = computeScore maybeDamage allianceScore membersNumber

    titanColor : TitanColor
    titanColor = getTitanColorFromRow row fixedTitanIndexes.colorIndex

    titanStars : Int
    titanStars = getIntFromRow row fixedTitanIndexes.starIndex 0
  in
    MemberTitanScore memberScore titanColor titanStars

extractTitanDataForMember : ( List ( List String ) ) -> Int -> Int -> String -> MemberTitanScores
extractTitanDataForMember data offset index memberPseudo =
  let
    memberDataIndex : Int
    memberDataIndex = offset + index

    memberScores : List MemberTitanScore
    memberScores = List.map ( extractMemberTitanScore memberDataIndex ) data

  in
    MemberTitanScores memberPseudo memberScores

extractTitanMemberScores : RawSheet -> List MemberTitanScores
extractTitanMemberScores titanSheet =
  let
    headerRow : List String
    headerRow = withDefault [] ( List.head titanSheet.values )

    data : List ( List String )
    data = List.drop 1 titanSheet.values

  in
    List.drop fixedTitanIndexes.number headerRow
              |> List.indexedMap ( extractTitanDataForMember data fixedTitanIndexes.number )

parseRawStats : RawStats -> Stats
parseRawStats rawStats =
  let
    titanSheet : RawSheet
    titanSheet = withDefault ( RawSheet [] ) ( getAt 0 rawStats.valueRanges )

    warSheet : RawSheet
    warSheet = withDefault ( RawSheet [] ) ( getAt 1 rawStats.valueRanges )

  in
    Stats
      ( extractDates titanSheet fixedTitanIndexes.number )
      ( extractDates warSheet fixedTitanIndexes.number )
      ( extractTitanAllianceScores titanSheet )
      ( extractWarAllianceScores warSheet )
      ( extractTitanMemberScores titanSheet )
      ( [] )

decodeStats : String -> Stats
decodeStats statsAsString = decodeRawStats statsAsString |> parseRawStats

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

