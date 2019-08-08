module Stats exposing (
  Stats, AllianceStats, MemberStats, FilteredStats, StatsExtender,
  fetchAllStats,
  updateStats, updateStatsWithFilter,
  viewAllianceStats, viewTitansStats, viewWarsStats
  )

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Maybe exposing (withDefault)
import ParseInt exposing (parseInt)

import Msg exposing (..)
import GetAt exposing (getAt)
import Quote exposing (quote)
import TakeLast exposing (takeLast)
import Spinner exposing (viewSpinner)
import MaybeExtra exposing (hasValue)
import CustomStyle exposing (customStyle)
import AllianceName exposing (allianceName)
import Gsheet exposing (computeSheetDataUrl)
import AreListsEqual exposing (areListsEqual)
import Wars exposing (sanitizeExternalWarBonus)
import ComputeTeamValue exposing (computeTeamValue)
import CreateBearerHeader exposing (createBearerHeader)
import MemberScore exposing (MemberScore, AverageMemberScore)
import MapWithPreviousAndNext exposing (mapWithPreviousAndNext)
import FindPreferredEventType exposing (findPreferredEventType)
import ComputeAverage exposing (computeAverageDamage, computeAverageScore)
import Titans exposing (DetailedColor, titanColorFromString, allTitanColors)
import GraphUtils exposing (getLineEndX, getLineEndY, getLineStartX, getLineStartY)
import StatsFilter exposing (StatsFilterExtender, defaultStatsFilter, viewTitanFilterForm)
import Gsheet exposing (RawStats, RawSheet, computeSheetDataUrl, decodeRawStats, fixedTitanIndexes, fixedWarIndexes)

------------
-- MODELS --
------------

type alias StatsExtender r =
  { r
  | stats: Maybe Stats
  , statsError : Maybe String
  , allianceStats: Maybe AllianceStats
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

type alias AllianceStats =
  { averageTitanScore : Float
  , maxTitanScore : Int
  , preferredTitanColor : DetailedColor
  , averageWarScore : Float
  , preferredWarBonus : String
  , memberStats : Dict String MemberStats
  }

type alias MemberStats =
  { pseudo : String
  , averageTitanScore : AverageMemberScore
  , maxTitanScore : Maybe Int
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
  , maxTitanScore : Int
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
  , maxScore : Maybe Int
  , preferredTitanColor : Maybe DetailedColor
  , titanScores : List MemberTitanScore
  }

type alias FilteredMemberWarScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , preferredWarBonus : Maybe String
  , warScores : List MemberWarScore
  }

type alias TitanScore =
  { score : Int
  , teamValue : Int
  , titanColor: DetailedColor
  , titanStars : Int
  }

----------
-- VIEW --
----------

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

viewAllianceStats : StatsExtender r -> Html Msg
viewAllianceStats { allianceStats } =
  case allianceStats of
    Just validAllianceStats -> viewValidAllianceStats validAllianceStats
    Nothing -> viewSpinner "Fetching the data"

viewTitansStats : StatsFilterExtender (StatsExtender r) -> Html Msg
viewTitansStats model =
  case model.filteredStats of
    Just validFilteredStats -> viewValidTitansStats model validFilteredStats
    Nothing -> viewSpinner "Fetching the data"

viewWarsStats : StatsFilterExtender (StatsExtender r) -> Html Msg
viewWarsStats model = div [] [ text "War stats coming soon" ] -- TODO

compareMembersStats : MemberStats -> MemberStats -> Order
compareMembersStats stats1 stats2 =
  if stats1.teamValue > stats2.teamValue then GT
  else if stats1.teamValue < stats2.teamValue then LT
  else compare stats1.pseudo stats2.pseudo

viewValidTitansStats : StatsFilterExtender r -> FilteredStats -> Html Msg
viewValidTitansStats statsFilter filteredStats =
  let
    dateRowHeadingElement : Html Msg
    dateRowHeadingElement = th [] [ text "Titan date" ]

    titanDatesElements : List (Html Msg)
    titanDatesElements = filteredStats.titanDates |> List.map ( \date -> th [] [ text date ] )

  in
    div [ class "stats" ] [
      viewTitanFilterForm statsFilter ( allianceName :: ( List.map .pseudo filteredStats.membersTitanScores ) ),
      div [ class "titan-stats" ] [
        div [ class "graphs-container" ] [
          div [ class "chart-title" ] [ text "Titan scores" ],
          div [ class "graph-container" ] [
            table [ class "chart", customStyle [ ("--titans", String.fromInt statsFilter.filteredTitanPeriod) ] ] [
              thead [] [ tr [] ( dateRowHeadingElement :: titanDatesElements ) ],
              tbody [] ( viewTitanScores statsFilter filteredStats )
            ]
          ]
        ]
      ]
    ]

generifyAllianceTitanScore : AllianceTitanScore -> MemberTitanScore
generifyAllianceTitanScore allianceTitanScore =
  { score = Just ( MemberScore allianceTitanScore.damage 0 )
  , titanColor = allianceTitanScore.titanColor
  , titanStars = allianceTitanScore.titanStars
  }

viewTitanScores : StatsFilterExtender r -> FilteredStats -> List ( Html Msg )
viewTitanScores statsFilter filteredStats =
  let
    allianceScores : Html Msg
    allianceScores =  viewGenericScores
      ( List.map generifyAllianceTitanScore filteredStats.allianceTitanScores.titanScores )
      allianceName
      ( statsFilter.filteredMember /= allianceName )
      filteredStats.allianceTitanScores.maxTitanScore

    membersScores : List ( Html Msg )
    membersScores = List.map ( viewMemberTitanScores statsFilter ) filteredStats.membersTitanScores
  in
    allianceScores :: membersScores

viewMemberTitanScores : StatsFilterExtender r -> FilteredMemberTitanScores -> Html Msg
viewMemberTitanScores statsFilters filteredMemberTitanScores = viewGenericScores
  filteredMemberTitanScores.titanScores
  filteredMemberTitanScores.pseudo
  ( statsFilters.filteredMember /= filteredMemberTitanScores.pseudo )
  ( withDefault 0 filteredMemberTitanScores.maxScore )

viewGenericScores : List MemberTitanScore -> String -> Bool -> Int -> Html Msg
viewGenericScores genericMemberScores memberPseudo isHidden maxDamage =
  let
    rowStyle : Attribute msg
    rowStyle = customStyle [
        ( "--max", String.fromInt maxDamage ),
        ( "--max-as-string", String.fromInt maxDamage |> quote )
      ]

  in
    tr [ hidden isHidden, rowStyle ]
      (
        ( th [ class "labels" ] [ text memberPseudo ] ) ::
        ( mapWithPreviousAndNext viewTitanScore genericMemberScores )
      )

viewTitanScore : ( Maybe MemberTitanScore, MemberTitanScore, Maybe MemberTitanScore ) -> Html Msg
viewTitanScore ( maybePreviousScore, currentScore, maybeNextScore ) =
  let
    scoreExtractor : MemberTitanScore -> Int
    scoreExtractor titanScore = titanScore.score |> withDefault { damage = 0, teamValue = 0 } |> .damage

    damage : Int
    damage = scoreExtractor currentScore

    maybePreviousDamage : Maybe Int
    maybePreviousDamage = Maybe.map scoreExtractor maybePreviousScore

    maybeNextDamage : Maybe Int
    maybeNextDamage = Maybe.map scoreExtractor maybeNextScore
  in
    td
      [ class "chart-value"
      , customStyle
        [ ("--value", String.fromInt damage)
        , ("--value-as-string", String.fromInt damage |> quote)
        , ("--titan-color", quote currentScore.titanColor.name)
        , ("--titan-color-code", currentScore.titanColor.code)
        , ("--titan-stars-as-string", String.fromInt currentScore.titanStars |> quote)
        , ("--line-start-x", getLineStartX maybePreviousDamage)
        , ("--line-start-y", getLineStartY maybePreviousDamage)
        , ("--line-end-x", getLineEndX maybeNextDamage)
        , ("--line-end-y", getLineEndY maybeNextDamage)
        ]
      ]
      [ span [class "score-presenter"] [ String.fromInt damage |> text ] ]


viewValidAllianceStats : AllianceStats -> Html Msg
viewValidAllianceStats allianceStats =
  div [ class "alliance" ] [
    div [ class "alliance-members" ] [
      h2 [] [ text "Alliance" ],
      div [ class "alliance-stats" ] [
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text "Average titan score" ],
          span [ class "alliance-stat-value" ] [ text ( round allianceStats.averageTitanScore |> String.fromInt ) ]
        ],
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text "Preferred titan color" ],
          span [ class "alliance-stat-value" ] [ text allianceStats.preferredTitanColor.name ]
        ],
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text "Average war score" ],
          span [ class "alliance-stat-value" ] [ text ( round allianceStats.averageWarScore |> String.fromInt ) ]
        ],
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text "Preferred war bonus" ],
          span [ class "alliance-stat-value" ] [ text allianceStats.preferredWarBonus ]
        ]
      ],
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
          Dict.values allianceStats.memberStats
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

safeParseInt : String -> Maybe Int
safeParseInt intAsString =
  case parseInt intAsString of
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
getTitanColorFromRow row index = getAt index row |> withDefault "" |> titanColorFromString

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
        ( getIntFromRow row fixedTitanIndexes.starIndex 0 )
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

allianceTitanColorPredicate : DetailedColor -> AllianceTitanScore -> Bool
allianceTitanColorPredicate filteredTitanColor { titanColor } = filteredTitanColor == allTitanColors || titanColor == filteredTitanColor

filterAllianceTitanScores : StatsFilterExtender r -> List AllianceTitanScore -> FilteredAllianceTitanScores
filterAllianceTitanScores statsFilter allianceTitanScores =
  let
    filteredAllianceScores : List AllianceTitanScore
    filteredAllianceScores = List.filter ( allianceTitanColorPredicate statsFilter.filteredTitanColor ) allianceTitanScores
      |> takeLast statsFilter.filteredTitanPeriod

    preferredTitanColor : DetailedColor
    preferredTitanColor = findPreferredEventType ( .name << .titanColor ) ( Just << .damage ) filteredAllianceScores
      |> withDefault ""
      |> titanColorFromString
  in
    { averageTitanScore = computeAverageDamage filteredAllianceScores
    , maxTitanScore = List.map .damage filteredAllianceScores |> List.maximum |> Maybe.withDefault 0
    , preferredTitanColor = preferredTitanColor
    , titanScores = filteredAllianceScores
    }

filterAllianceWarScores : StatsFilterExtender r -> List AllianceWarScore -> FilteredAllianceWarScores
filterAllianceWarScores statsFilter allianceWarScores =
  let
    filteredAllianceScores : List AllianceWarScore
    filteredAllianceScores = takeLast statsFilter.filteredWarPeriod allianceWarScores

    preferredWarBonus : String
    preferredWarBonus = findPreferredEventType ( .warBonus ) ( Just << .damage ) filteredAllianceScores
      |> withDefault ""
      |> sanitizeExternalWarBonus
  in
    { averageWarScore = computeAverageDamage filteredAllianceScores
    , preferredWarBonus = preferredWarBonus
    , warScores = filteredAllianceScores
    }

memberTitanColorPredicate : DetailedColor -> MemberTitanScore -> Bool
memberTitanColorPredicate filteredTitanColor { titanColor } = filteredTitanColor == allTitanColors || titanColor == filteredTitanColor

filterMemberTitanScores : StatsFilterExtender r -> MemberTitanScores -> FilteredMemberTitanScores
filterMemberTitanScores statsFilter memberTitanScores =
  let
    filteredMemberScores : List MemberTitanScore
    filteredMemberScores = List.filter ( memberTitanColorPredicate statsFilter.filteredTitanColor ) memberTitanScores.titanScores
      |> takeLast statsFilter.filteredTitanPeriod

    preferredTitanColor : Maybe DetailedColor
    preferredTitanColor = findPreferredEventType ( .name << .titanColor ) ( ( Maybe.map .damage ) << .score ) filteredMemberScores
      |> Maybe.map titanColorFromString
  in
    { pseudo = memberTitanScores.pseudo
    , averageScore = List.map .score filteredMemberScores |> computeAverageScore
    , maxScore = List.map .score filteredMemberScores
      |> List.filter hasValue
      |> List.map ( Maybe.map .damage )
      |> List.map ( Maybe.withDefault 0 ) -- Default value not reachable, cf filter
      |> List.maximum
    , preferredTitanColor = preferredTitanColor
    , titanScores = filteredMemberScores
    }

filterMemberWarScores : StatsFilterExtender r -> MemberWarScores -> FilteredMemberWarScores
filterMemberWarScores statsFilter memberWarScores =
  let
    filteredMemberScores : List MemberWarScore
    filteredMemberScores = takeLast statsFilter.filteredTitanPeriod memberWarScores.warScores

    preferredWarBonus : Maybe String
    preferredWarBonus = findPreferredEventType ( .warBonus ) ( ( Maybe.map .damage ) << .score ) filteredMemberScores
      |> Maybe.map sanitizeExternalWarBonus
  in
    { pseudo = memberWarScores.pseudo
    , averageScore = List.map .score filteredMemberScores |> computeAverageScore
    , preferredWarBonus = preferredWarBonus
    , warScores = filteredMemberScores
    }

filterStats : StatsFilterExtender r -> Stats -> FilteredStats
filterStats statsFilter stats =
  { titanDates = takeLast statsFilter.filteredTitanPeriod stats.titanDates
  , warDates = takeLast statsFilter.filteredWarPeriod stats.warDates
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
    , maxTitanScore = memberTitanScores.maxScore
    , preferredTitanColor = memberTitanScores.preferredTitanColor
    , averageWarScore = memberWarScores.averageScore
    , preferredWarBonus = memberWarScores.preferredWarBonus
    , teamValue = mergeTeamValues memberTitanScores.averageScore memberWarScores.averageScore
    }
  )

computeAllianceStats : FilteredStats -> AllianceStats
computeAllianceStats filteredStats =
  { averageTitanScore = filteredStats.allianceTitanScores.averageTitanScore
  , maxTitanScore = filteredStats.allianceTitanScores.maxTitanScore
  , preferredTitanColor = filteredStats.allianceTitanScores.preferredTitanColor
  , averageWarScore = filteredStats.allianceWarScores.averageWarScore
  , preferredWarBonus = filteredStats.allianceWarScores.preferredWarBonus
  , memberStats = List.indexedMap
      (\index titanScores -> ( titanScores.pseudo, titanScores, retrieveMemberWarScores index filteredStats.membersWarScores ) ) filteredStats.membersTitanScores
      |> List.map computeMemberStats
      |> Dict.fromList
  }

updateValidStats : Stats -> StatsExtender r -> StatsExtender r
updateValidStats stats model =
  let
    filteredStats : FilteredStats
    filteredStats = filterStatsForAlliancePage stats

    allianceStats : AllianceStats
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

updateStatsWithFilter : StatsExtender ( StatsFilterExtender r ) -> StatsExtender ( StatsFilterExtender r )
updateStatsWithFilter model =
  { model
  | filteredStats = Maybe.map ( filterStats model ) model.stats
  }

