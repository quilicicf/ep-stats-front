module Stats exposing (
  Stats, MemberStats, FilteredStats, StatsExtender,
  fetchAllStats,
  updateStats,
  viewAllianceStats
  )

import Debug exposing (log)
import Dict exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http exposing (..)

import Json.Decode as Decode exposing(Value, Decoder, string)
import Json.Decode.Pipeline exposing (required)

import List.Extra exposing (getAt)

import Maybe exposing (withDefault)

import ParseInt exposing (parseInt)

import Regex exposing (..)

import String.Interpolate exposing (interpolate)

import Url exposing (..)

import Msg exposing (..)
import MemberScore exposing (MemberScore, AverageMemberScore)
import CustomStyle exposing (customStyle)
import AreListsEqual exposing (areListsEqual)
import Wars exposing (sanitizeExternalWarBonus)
import ComputeTeamValue exposing (computeTeamValue)
import CreateQueryString exposing (createQueryString)
import Titans exposing (DetailedColor, titanColorFromString)
import StatsFilter exposing (StatsFilterExtender, defaultStatsFilter)
import ComputeAverage exposing (computeAverageDamage, computeAverageScore)

------------
-- MODELS --
------------

type alias StatsExtender r =
  { r
  | stats: Maybe Stats
  , statsError : Maybe String
  , allianceStats: Maybe ( Dict String MemberStats )
  , filteredStats: Maybe FilteredStats
  }

type alias Stats =
  { titanDates : List String
  , warDates : List String
  , allianceTitanScores : List AllianceTitanScore
  , allianceWarScores: List AllianceWarScore
  , membersTitanScores: List MemberTitanScores
  , membersWarScores: List MemberWarScores
  }

type alias MemberStats =
  { pseudo : String
  , averageTitanScore : AverageMemberScore
  , preferredTitanColor : Maybe DetailedColor
  , averageWarScore : AverageMemberScore
  , preferredWarBonus : Maybe String
  , teamValue : Float
  }

type alias AllianceTitanScore =
  { damage : Int
  , titanColor : DetailedColor
  , titanStars : Int
  }

type alias AllianceWarScore =
  { damage : Int
  , warBonus : String
  }

type alias MemberTitanScores =
  { pseudo : String
  , titanScores : List MemberTitanScore
  }

type alias MemberTitanScore =
  { score : Maybe MemberScore
  , titanColor : DetailedColor
  , titanStars : Int
  }

type alias MemberWarScores =
  { pseudo : String
  , warScores : List MemberWarScore
  }

type alias MemberWarScore =
  { score : Maybe MemberScore
  , warBonus : String
  }

type alias FilteredStats =
  { titanDates : List String
  , warDates : List String
  , allianceTitanScores : FilteredAllianceTitanScores
  , allianceWarScores: FilteredAllianceWarScores
  , membersTitanScores: List FilteredMemberTitanScores
  , membersWarScores: List FilteredMemberWarScores
  }

type alias FilteredAllianceTitanScores =
  { averageTitanScore : Float
  , preferredTitanColor : DetailedColor
  , titanScores : List AllianceTitanScore
  }

type alias FilteredAllianceWarScores =
  { averageWarScore : Float
  , preferredWarBonus : String
  , warScores : List AllianceWarScore
  }

type alias FilteredMemberTitanScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , preferredTitanColor : Maybe DetailedColor
  , titanScores : List MemberTitanScore
  }

type alias FilteredMemberWarScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , preferredWarBonus : Maybe String
  , warScores : List MemberWarScore
  }

type alias RawStats = { valueRanges : List RawSheet }

type alias RawSheet = { values: List ( List String ) }

-- TITANS

type alias TitanScore =
  { score : Int
  , teamValue : Int
  , titanColor: DetailedColor
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

fixedWarIndexes =
  { number = 5
  , dateIndex = 0
  , totalIndex = 1
  , enemyScoreIndex = 2
  , bonusIndex = 3
  , membersIndex = 4
  }

-----------
-- UTILS --
-----------

-- WARS

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

statsSpinner : Html Msg
statsSpinner =
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

viewAllianceStats : StatsExtender r -> Html Msg
viewAllianceStats { allianceStats } =
  case allianceStats of
    Just validAllianceStats -> viewValidAllianceStats validAllianceStats
    Nothing -> statsSpinner

compareMembersStats : MemberStats -> MemberStats -> Order
compareMembersStats stats1 stats2 =
  if stats1.teamValue > stats2.teamValue then GT
  else if stats1.teamValue < stats2.teamValue then LT
  else compare stats1.pseudo stats2.pseudo


viewValidAllianceStats : Dict String MemberStats -> Html Msg
viewValidAllianceStats allianceStats =
  div [ class "alliance" ] [
    div [ class "alliance-members" ] [
      h2 [] [ text "Alliance members" ],
      table [] [
        thead [] [
          th [] [ text "Pseudo" ],
          th [] [ text "Average titan score" ],
          th [] [ text "Preferred titan color" ],
          th [] [ text "Average war score" ],
          th [] [ text "Preferred war type" ],
          th [] [ text "Team value" ]
        ],
        tbody [] (
          Dict.values allianceStats
            |> List.sortWith compareMembersStats
            |> List.reverse
            |> List.map viewMember
        )
      ]
    ]
  ]

viewMember : MemberStats -> Html Msg
viewMember memberStats =
  tr [ class "member-row" ] [
    th [ class "member-pseudo" ] [ text memberStats.pseudo ],
    td
      [ class "member-value", addCompletenessClass memberStats.averageTitanScore.isComplete ]
      [ text ( round memberStats.averageTitanScore.damage |> String.fromInt ) ],
    td [ class "member-value" ] [ text ( Maybe.map .name memberStats.preferredTitanColor  |> withDefault "N/A") ],
    td
      [ class "member-value", addCompletenessClass memberStats.averageWarScore.isComplete ]
      [ text ( round memberStats.averageWarScore.damage |> String.fromInt ) ],
    td [ class "member-value" ] [ text ( withDefault "N/A" memberStats.preferredWarBonus ) ],
    td [ class "member-value" ] [ text ( round memberStats.teamValue |> String.fromInt ) ]
  ]

addCompletenessClass : Bool -> Attribute msg
addCompletenessClass isComplete = class (
    if isComplete then "value-complete"
    else "value-incomplete"
  )

------------
-- UPDATE --
------------

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
    Just damage -> Just ( MemberScore damage ( computeTeamValue allianceScore membersNumber damage ) )
    Nothing -> Nothing

getMaybeIntFromRow : List String -> Int -> Maybe Int
getMaybeIntFromRow row index = getAt index row |> Maybe.andThen safeParseInt

getIntFromRow : List String -> Int -> Int -> Int
getIntFromRow row index default = getAt index row |> Maybe.andThen safeParseInt |> withDefault default

getTitanColorFromRow : List String -> Int -> DetailedColor
getTitanColorFromRow row index = getAt index row |> withDefault ""|> titanColorFromString

getWarBonusFromRow : List String -> Int -> String
getWarBonusFromRow row index = getAt index row |> withDefault "" |> sanitizeExternalWarBonus

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
        ( getIntFromRow row fixedWarIndexes.totalIndex 0 )
        ( getWarBonusFromRow row fixedWarIndexes.bonusIndex )
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

    titanColor : DetailedColor
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

extractMemberWarScore : Int -> List String -> MemberWarScore
extractMemberWarScore memberIndex row =
  let
    maybeDamage : Maybe Int
    maybeDamage = getMaybeIntFromRow row memberIndex

    allianceScore : Int
    allianceScore = getIntFromRow row fixedWarIndexes.totalIndex 0

    membersNumber : Int
    membersNumber = getIntFromRow row fixedWarIndexes.membersIndex 0

    memberScore : Maybe MemberScore
    memberScore = computeScore maybeDamage allianceScore membersNumber

    warBonus : String
    warBonus = getWarBonusFromRow row fixedWarIndexes.bonusIndex
  in
    MemberWarScore memberScore warBonus

extractWarDataForMember : ( List ( List String ) ) -> Int -> Int -> String -> MemberWarScores
extractWarDataForMember data offset index memberPseudo =
  let
    memberDataIndex : Int
    memberDataIndex = offset + index

    memberScores : List MemberWarScore
    memberScores = List.map ( extractMemberWarScore memberDataIndex ) data

  in
    MemberWarScores memberPseudo memberScores

extractWarMemberScores : RawSheet -> List MemberWarScores
extractWarMemberScores warSheet =
  let
    headerRow : List String
    headerRow = withDefault [] ( List.head warSheet.values )

    data : List ( List String )
    data = List.drop 1 warSheet.values

  in
    List.drop fixedWarIndexes.number headerRow
              |> List.indexedMap ( extractWarDataForMember data fixedWarIndexes.number )

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
      ( extractWarMemberScores warSheet )

filterByPeriod : Int -> List a -> List a
filterByPeriod period list = List.reverse list |> List.take period |> List.reverse

filterAllianceTitanScores : StatsFilterExtender r -> List AllianceTitanScore -> FilteredAllianceTitanScores
filterAllianceTitanScores statsFilter allianceTitanScores =
  let
    filteredAllianceScores : List AllianceTitanScore
    filteredAllianceScores = filterByPeriod statsFilter.filteredTitanPeriod allianceTitanScores
  in
    { averageTitanScore = computeAverageDamage filteredAllianceScores
    , preferredTitanColor = DetailedColor "RED" "var(--red)" -- TODO find the real one
    , titanScores = filteredAllianceScores
    }

filterAllianceWarScores : StatsFilterExtender r -> List AllianceWarScore -> FilteredAllianceWarScores
filterAllianceWarScores statsFilter allianceWarScores =
  let
    filteredAllianceScores : List AllianceWarScore
    filteredAllianceScores = filterByPeriod statsFilter.filteredWarPeriod allianceWarScores
  in
    { averageWarScore = computeAverageDamage filteredAllianceScores
    , preferredWarBonus = "ATTACK" -- TODO find the real one
    , warScores = filteredAllianceScores
    }

filterMemberTitanScores : StatsFilterExtender r -> MemberTitanScores -> FilteredMemberTitanScores
filterMemberTitanScores statsFilter memberTitanScores =
  let
    filteredMemberScores : List MemberTitanScore
    filteredMemberScores = filterByPeriod statsFilter.filteredTitanPeriod memberTitanScores.titanScores
  in
    { pseudo = memberTitanScores.pseudo
    , averageScore = List.map .score filteredMemberScores |> computeAverageScore
    , preferredTitanColor = Nothing -- TODO find the real one
    , titanScores = filteredMemberScores
    }

filterMemberWarScores : StatsFilterExtender r -> MemberWarScores -> FilteredMemberWarScores
filterMemberWarScores statsFilter memberWarScores =
  let
    filteredMemberScores : List MemberWarScore
    filteredMemberScores = filterByPeriod statsFilter.filteredTitanPeriod memberWarScores.warScores
  in
    { pseudo = memberWarScores.pseudo
    , averageScore = List.map .score filteredMemberScores |> computeAverageScore
    , preferredWarBonus = Nothing -- TODO find the real one
    , warScores = filteredMemberScores
    }

filterStats : StatsFilterExtender r -> Stats -> FilteredStats
filterStats statsFilter stats =
  { titanDates = filterByPeriod statsFilter.filteredTitanPeriod stats.titanDates
  , warDates = filterByPeriod statsFilter.filteredWarPeriod stats.warDates
  , allianceTitanScores = filterAllianceTitanScores statsFilter stats.allianceTitanScores
  , allianceWarScores = filterAllianceWarScores statsFilter stats.allianceWarScores
  , membersTitanScores = List.map ( filterMemberTitanScores statsFilter ) stats.membersTitanScores
  , membersWarScores = List.map ( filterMemberWarScores statsFilter ) stats.membersWarScores
  }

filterStatsForAlliancePage : Stats -> FilteredStats
filterStatsForAlliancePage stats = filterStats defaultStatsFilter stats

decodeStats : String -> Stats
decodeStats statsAsString = decodeRawStats statsAsString |> parseRawStats

retrieveMemberWarScores : Int -> List ( FilteredMemberWarScores ) -> FilteredMemberWarScores
retrieveMemberWarScores index memberWarScoresList = Maybe.withDefault
  ( FilteredMemberWarScores "" ( AverageMemberScore False 0 0 ) Nothing [] ) -- Can't fail anyway, checked before that
  ( getAt index memberWarScoresList )

mergeTeamValues : AverageMemberScore -> AverageMemberScore -> Float
mergeTeamValues titanAverageScore warAverageScore = titanAverageScore.teamValue + warAverageScore.teamValue / 2

computeMemberStats : (String, FilteredMemberTitanScores, FilteredMemberWarScores) -> ( String, MemberStats )
computeMemberStats ( pseudo, memberTitanScores, memberWarScores ) =
  (
    pseudo,
    { pseudo = pseudo
    , averageTitanScore = memberTitanScores.averageScore
    , preferredTitanColor = memberTitanScores.preferredTitanColor
    , averageWarScore = memberWarScores.averageScore
    , preferredWarBonus = memberWarScores.preferredWarBonus
    , teamValue = mergeTeamValues memberTitanScores.averageScore memberWarScores.averageScore
    }
  )

computeAllianceStats : FilteredStats -> Dict String MemberStats
computeAllianceStats filteredStats =
  List.indexedMap (\index titanScores -> ( titanScores.pseudo, titanScores, retrieveMemberWarScores index filteredStats.membersWarScores ) ) filteredStats.membersTitanScores
    |> List.map computeMemberStats
    |> Dict.fromList

updateValidStats : Stats -> StatsExtender r -> StatsExtender r
updateValidStats stats model =
  let
    filteredStats : FilteredStats
    filteredStats = filterStatsForAlliancePage stats

    allianceStats : Dict String MemberStats
    allianceStats = computeAllianceStats filteredStats
  in
    { model
    | stats = Just stats
    , allianceStats = Just allianceStats
    , filteredStats = Just filteredStats
    }

decodeAndUpdateStats : String -> StatsExtender r -> StatsExtender r
decodeAndUpdateStats statsAsString model =
  let
    stats : Stats
    stats = decodeStats statsAsString

    areMembersListEqual : Bool
    areMembersListEqual = areListsEqual
      ( List.map .pseudo stats.membersTitanScores )
      ( List.map .pseudo stats.membersWarScores )

    maybeErrorMessage : Maybe String
    maybeErrorMessage = if areMembersListEqual then Nothing else Just "The members lists differ in the Titans and Wars tabs"

  in
    case maybeErrorMessage of
      Nothing -> updateValidStats stats model
      Just _ -> { model | statsError = maybeErrorMessage }

updateStats : StatsMsg -> StatsExtender r -> StatsExtender r
updateStats msg model =
  case msg of
    GotStats httpResult ->
      case httpResult of
        Ok statsAsString -> decodeAndUpdateStats statsAsString model
        Err _ -> model

