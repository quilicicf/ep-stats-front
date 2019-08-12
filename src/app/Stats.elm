module Stats exposing (
  Stats, AllianceStats, MemberStats, FilteredStats, StatsExtender,
  fetchAllStats,
  updateStats, updateStatsWithFilter,
  viewAllianceStats, viewTitansStats, viewWarsStats
  )

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http exposing (..)
import Maybe exposing (withDefault)

import Msg exposing (..)
import Flip exposing (flip)
import GetAt exposing (getAt)
import Quote exposing (quote)
import TakeLast exposing (takeLast)
import Spinner exposing (viewSpinner)
import MaybeExtra exposing (hasValue)
import CustomStyle exposing (customStyle)
import PresentDate exposing (presentDate)
import AllianceName exposing (allianceName)
import Gsheet exposing (computeSheetDataUrl)
import PresentNumber exposing (presentNumber)
import AreListsEqual exposing (areListsEqual)
import Wars exposing (sanitizeExternalWarBonus)
import ComputeTeamValue exposing (computeTeamValue)
import CreateBearerHeader exposing (createBearerHeader)
import MemberScore exposing (MemberScore, AverageMemberScore)
import MapWithPreviousAndNext exposing (mapWithPreviousAndNext)
import FindPreferredEventType exposing (findPreferredEventType)
import ComputeAverage exposing (computeAverageDamage, computeAverageScore)
import LinearRegression exposing (RegressionResult, computeLinearRegression)
import Titans exposing (DetailedColor, titanColorFromString, allTitanColors)
import GraphUtils exposing (getLineEndX, getLineEndY, getLineStartX, getLineStartY)
import StatsFilter exposing (StatsFilterExtender, defaultStatsFilter, viewTitansFilterForm, viewWarsFilterForm)
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
  { allianceTitanScores : List AllianceTitanScore
  , allianceWarScores: List AllianceWarScore
  , membersTitanScores: List MemberTitanScores
  , membersWarScores: List MemberWarScores
  }

type alias AllianceStats =
  { averageTitanScore : Float
  , maxTitanScore : Int
  , preferredTitanColor : DetailedColor
  , averageWarScore : Float
  , maxWarScore : Int
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
  { date : String
  , damage : Int
  , titanColor : DetailedColor
  , titanStars : Int
  }

type alias AllianceWarScore =
  { date : String
  , damage : Int
  , warBonus : String
  }

type alias MemberTitanScores =
  { pseudo : String
  , titanScores : List MemberTitanScore
  }

type alias MemberTitanScore =
  { date : String
  , score : Maybe MemberScore
  , titanColor : DetailedColor
  , titanStars : Int
  }

type alias MemberWarScores =
  { pseudo : String
  , warScores : List MemberWarScore
  }

type alias MemberWarScore =
  { date : String
  , score : Maybe MemberScore
  , warBonus : String
  }

type alias FilteredStats =
  { allianceTitanScores : FilteredAllianceTitanScores
  , allianceWarScores: FilteredAllianceWarScores
  , membersTitanScores: List FilteredMemberTitanScores
  , membersWarScores: List FilteredMemberWarScores
  }

type alias FilteredAllianceTitanScores =
  { averageTitanScore : Float
  , maxTitanScore : Int
  , progression : RegressionResult
  , preferredTitanColor : DetailedColor
  , titanScores : List AllianceTitanScore
  }

type alias FilteredAllianceWarScores =
  { averageWarScore : Float
  , maxWarScore : Int
  , preferredWarBonus : String
  , warScores : List AllianceWarScore
  }

type alias FilteredMemberTitanScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , maxScore : Maybe Int
  , progression : RegressionResult
  , preferredTitanColor : Maybe DetailedColor
  , titanScores : List MemberTitanScore
  }

type alias FilteredMemberWarScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , maxScore : Maybe Int
  , preferredWarBonus : Maybe String
  , warScores : List MemberWarScore
  }

type alias TitanScore =
  { score : Int
  , teamValue : Int
  , titanColor: DetailedColor
  , titanStars : Int
  }

type alias LineData msg =
  { lineType : String
  , lineStyle : Attribute msg
  }

---------------
-- CONSTANTS --
---------------

graphRatio : Int
graphRatio = 9

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
viewWarsStats model =
  case model.filteredStats of
    Just validFilteredStats -> viewValidWarsStats model validFilteredStats
    Nothing -> viewSpinner "Fetching the data"

compareMembersStats : MemberStats -> MemberStats -> Order
compareMembersStats stats1 stats2 =
  if stats1.teamValue > stats2.teamValue then GT
  else if stats1.teamValue < stats2.teamValue then LT
  else compare stats1.pseudo stats2.pseudo

computeTendencyStyle : Int -> FilteredMemberTitanScores -> Attribute msg
computeTendencyStyle maxDamage genericTitanScores =
  let
    predict : Int -> Float
    predict = List.map .score genericTitanScores.titanScores
      |> List.map (Maybe.map .damage)
      |> List.map (withDefault 0)
      |> List.indexedMap (\index damage -> [index, damage])
      |> computeLinearRegression 2
      |> .predict

    startRatio : Float
    startRatio = predict 0 |> flip (/) (toFloat maxDamage)

    endRatio : Float
    endRatio = predict (List.length genericTitanScores.titanScores)
      |> flip (/) (toFloat maxDamage)

    slope : Float
    slope = computeSlope startRatio endRatio / (toFloat (List.length genericTitanScores.titanScores))

    offsetRatio : Float
    offsetRatio = computeOffsetRatio slope
  in
    customStyle [
        ("--start-percent-to-max", startRatio |> percentAsCss),
        ("--end-percent-to-max", endRatio |> percentAsCss),
        ("--offset-ratio", String.fromFloat offsetRatio)
      ]

viewValidTitansStats : StatsFilterExtender r -> FilteredStats -> Html Msg
viewValidTitansStats statsFilter filteredStats =
  let
    genericTitanScores : FilteredMemberTitanScores
    genericTitanScores = List.filter (\memberScores -> statsFilter.filteredMember == memberScores.pseudo) filteredStats.membersTitanScores
      |> List.head
      |> withDefault (generifyAllianceTitanScores filteredStats.allianceTitanScores)

    maxDamage : Int
    maxDamage = withDefault 0 genericTitanScores.maxScore

    eventsNumberStyle : Attribute msg
    eventsNumberStyle = customStyle [
        ( "--events-number", List.length filteredStats.allianceTitanScores.titanScores |> String.fromInt )
      ]

  in
    div [ class "stats" ] [
      viewTitansFilterForm statsFilter ( allianceName :: ( List.map .pseudo filteredStats.membersTitanScores ) ),
      div [ class "titan-stats" ] [
        div [ class "graphs-container" ] [
          h2 [ class "chart-title" ] [ text "Titan scores" ],
          div [ class "graph-container", ariaHidden True, eventsNumberStyle ] [
            div [ class "chart"] [
              div [ class "label max-label" ] [
                span [ class "label-value" ] [ withDefault 0 genericTitanScores.maxScore |> presentNumber |> text ]
              ],
              div [ class "label zero-label" ] [
                span [ class "label-value" ] [ text "0" ]
              ],
              div [ class "member-stats" ] (
                viewGenericTitanScores genericTitanScores ++ [
                  div [ class "line tendency", computeTendencyStyle maxDamage genericTitanScores ] []
                ]
              )
            ]
          ]
        ]
      ]
    ]

viewValidWarsStats : StatsFilterExtender r -> FilteredStats -> Html Msg
viewValidWarsStats statsFilter filteredStats =
  let
    dateRowHeadingElement : Html Msg
    dateRowHeadingElement = th [] [ text "War date" ]

  in
    div [ class "stats" ] [
      viewWarsFilterForm statsFilter ( allianceName :: ( List.map .pseudo filteredStats.membersWarScores ) ),
      div [ class "war-stats" ] [
        div [ class "graphs-container" ] [
          div [ class "chart-title" ] [ text "War scores" ],
          div [ class "graph-container" ] [
            table [ class "chart" ] [
              thead [] [ tr [] ( dateRowHeadingElement :: [] ) ],
              tbody [] ( viewWarScores statsFilter filteredStats )
            ]
          ]
        ]
      ]
    ]

generifyAllianceTitanScores : FilteredAllianceTitanScores -> FilteredMemberTitanScores
generifyAllianceTitanScores allianceTitanScores =
  { pseudo = allianceName
  , averageScore = AverageMemberScore True allianceTitanScores.averageTitanScore 0
  , maxScore = Just allianceTitanScores.maxTitanScore
  , progression = allianceTitanScores.progression
  , preferredTitanColor = Just allianceTitanScores.preferredTitanColor
  , titanScores = List.map generifyAllianceTitanScore allianceTitanScores.titanScores
  }

generifyAllianceTitanScore : AllianceTitanScore -> MemberTitanScore
generifyAllianceTitanScore allianceTitanScore =
  { date = allianceTitanScore.date
  , score = Just ( MemberScore allianceTitanScore.damage 0 )
  , titanColor = allianceTitanScore.titanColor
  , titanStars = allianceTitanScore.titanStars
  }

generifyAllianceWarScore : AllianceWarScore -> MemberWarScore
generifyAllianceWarScore allianceWarScore =
  { date = allianceWarScore.date
  , score = Just ( MemberScore allianceWarScore.damage 0 )
  , warBonus = allianceWarScore.warBonus
  }

viewWarScores : StatsFilterExtender r -> FilteredStats -> List ( Html Msg )
viewWarScores statsFilter filteredStats =
  let
    allianceScores : Html Msg
    allianceScores =  viewGenericWarScores
      ( List.map generifyAllianceWarScore filteredStats.allianceWarScores.warScores )
      allianceName
      ( statsFilter.filteredMember /= allianceName )
      filteredStats.allianceWarScores.maxWarScore

    membersScores : List ( Html Msg )
    membersScores = List.map ( viewMemberWarScores statsFilter ) filteredStats.membersWarScores
  in
    allianceScores :: membersScores

viewMemberWarScores : StatsFilterExtender r -> FilteredMemberWarScores -> Html Msg
viewMemberWarScores statsFilters filteredMemberWarScores = viewGenericWarScores
  filteredMemberWarScores.warScores
  filteredMemberWarScores.pseudo
  ( statsFilters.filteredMember /= filteredMemberWarScores.pseudo )
  ( withDefault 0 filteredMemberWarScores.maxScore )

viewGenericTitanScores : FilteredMemberTitanScores -> List (Html Msg)
viewGenericTitanScores scores = mapWithPreviousAndNext ( viewGenericTitanScore ( withDefault 0 scores.maxScore )) scores.titanScores

computeSlope : Float -> Float -> Float
computeSlope ratio1 ratio2 = atan ((toFloat graphRatio) * (ratio2 - ratio1) / 2)

toRadians : Int -> Float
toRadians angleInDegrees = (toFloat angleInDegrees) * pi / 180

computeOffsetRatio : Float -> Float
computeOffsetRatio slope = (1 / sin(toRadians(90) - slope)) ^ 2

percentAsCss : Float -> String
percentAsCss ratio = ratio * 10000.0
  |> round
  |> toFloat
  |> flip (/) 100
  |> String.fromFloat
  |> flip (++) "%"

emptyLineData : LineData msg
emptyLineData = { lineType = "no-line", lineStyle = customStyle [] }

computeIncomingLineData : Maybe { r | score: Maybe MemberScore } -> Float -> Int -> LineData msg
computeIncomingLineData maybePreviousScore currentPercent maxDamage =
  case maybePreviousScore of
    Nothing -> emptyLineData
    Just previousScore ->
      let
        previousDamage : Int
        previousDamage = Maybe.map .damage previousScore.score |> withDefault 0

        previousPercent : Float
        previousPercent = toFloat previousDamage / toFloat maxDamage

        lineType : String
        lineType = if previousPercent < currentPercent then "rising-line" else "dropping-line"

        percentBetween : Float
        percentBetween = (previousPercent + currentPercent) / 2

        slope : Float
        slope = computeSlope percentBetween currentPercent

        lineStyle : Attribute msg
        lineStyle = customStyle [
            ("--start-percent-to-max", percentAsCss percentBetween),
            ("--end-percent-to-max", percentAsCss currentPercent),
            ("--offset-ratio", computeOffsetRatio slope |> String.fromFloat)
          ]

      in
        { lineType = "line graph-line incoming-line " ++ lineType
        , lineStyle = lineStyle
        }

computeOutgoingLineData : Maybe { r | score: Maybe MemberScore } -> Float -> Int -> LineData msg
computeOutgoingLineData maybeNextScore currentPercent maxDamage =
  case maybeNextScore of
    Nothing -> emptyLineData
    Just nextScore ->
      let
        nextDamage : Int
        nextDamage = Maybe.map .damage nextScore.score |> withDefault 0

        nextPercent : Float
        nextPercent = toFloat nextDamage / toFloat maxDamage

        lineType : String
        lineType = if nextPercent > currentPercent then "rising-line" else "dropping-line"

        percentBetween : Float
        percentBetween = (nextPercent + currentPercent) / 2

        slope : Float
        slope = computeSlope currentPercent percentBetween

        lineStyle : Attribute msg
        lineStyle = customStyle [
            ("--start-percent-to-max", percentAsCss currentPercent),
            ("--end-percent-to-max", percentAsCss percentBetween),
            ("--offset-ratio", computeOffsetRatio slope |> String.fromFloat)
          ]

      in
        { lineType = "line graph-line outgoing-line " ++ lineType
        , lineStyle = lineStyle
        }

viewGenericTitanScore : Int -> (Maybe MemberTitanScore, MemberTitanScore, Maybe MemberTitanScore) -> Html Msg
viewGenericTitanScore maxDamage (maybePreviousScore, currentScore, maybeNextScore) =
  let
    currentDamage : Int
    currentDamage = Maybe.map .damage currentScore.score |> withDefault 0

    titanColorName : String
    titanColorName = currentScore.titanColor.name

    percentToMax : Float
    percentToMax = toFloat currentDamage / toFloat maxDamage

    percentFromMax : Float
    percentFromMax = 1 - percentToMax

    percentFromMaxStyle : Attribute Msg
    percentFromMaxStyle = customStyle [ ( "--ratio-from-max", String.fromFloat percentFromMax ) ]

    currentPercent : Float
    currentPercent = toFloat currentDamage / toFloat maxDamage

    incomingLineData : LineData msg
    incomingLineData = computeIncomingLineData maybePreviousScore currentPercent maxDamage

    outgoingLineData : LineData msg
    outgoingLineData = computeOutgoingLineData maybeNextScore currentPercent maxDamage

  in
    div [ class "member-stat", percentFromMaxStyle ] [
      div [ class incomingLineData.lineType, incomingLineData.lineStyle ] [],
      div [ class "member-damage" ] [
        div [ class ("bullet-point titan-color-" ++ (titanColorName |> String.toLower)) ] [
          i [ class currentScore.titanColor.icon ] []
        ],
        div [ class "tooltip-trigger" ] [
          p [ class "tooltip" ] [
            span [] [
              text ( String.join " " [ titanColorName, String.fromInt currentScore.titanStars ] ),
              i [ class "fas fa-star" ] []
            ],
            br [] [],
            span [] [ presentNumber currentDamage |> text ],
            br [] [],
            span [] [ presentDate currentScore.date |> text ]
          ]
        ]
      ],
      div [ class outgoingLineData.lineType, outgoingLineData.lineStyle ] []
    ]

viewGenericWarScores : List MemberWarScore -> String -> Bool -> Int -> Html Msg
viewGenericWarScores genericMemberScores memberPseudo isHidden maxDamage =
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
        ( mapWithPreviousAndNext viewWarScore genericMemberScores )
      )

viewWarScore : ( Maybe MemberWarScore, MemberWarScore, Maybe MemberWarScore ) -> Html Msg
viewWarScore ( maybePreviousScore, currentScore, maybeNextScore ) =
  let
    scoreExtractor : MemberWarScore -> Int
    scoreExtractor warScore = warScore.score |> withDefault { damage = 0, teamValue = 0 } |> .damage

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
        , ("--war-bonus", quote currentScore.warBonus)
        , ("--line-start-x", getLineStartX maybePreviousDamage)
        , ("--line-start-y", getLineStartY maybePreviousDamage)
        , ("--line-end-x", getLineEndX maybeNextDamage)
        , ("--line-end-y", getLineEndY maybeNextDamage)
        , ("--event", (quote currentScore.warBonus))
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

computeScore : Maybe Int -> Int -> Int -> Maybe MemberScore
computeScore maybeDamage allianceScore membersNumber =
  case maybeDamage of
    Just damage -> Just ( MemberScore damage ( computeTeamValue allianceScore membersNumber damage ) )
    Nothing -> Nothing

getMaybeIntFromRow : List String -> Int -> Maybe Int
getMaybeIntFromRow row index = getAt index row |> Maybe.andThen String.toInt

getIntFromRow : List String -> Int -> Int -> Int
getIntFromRow row index default = getAt index row |> Maybe.andThen String.toInt |> withDefault default

getTitanColorFromRow : List String -> Int -> DetailedColor
getTitanColorFromRow row index = getAt index row |> withDefault "" |> titanColorFromString

getWarBonusFromRow : List String -> Int -> String
getWarBonusFromRow row index = getAt index row |> withDefault "" |> sanitizeExternalWarBonus

extractTitanAllianceScores : RawSheet -> List AllianceTitanScore
extractTitanAllianceScores titanSheet =
  List.drop 1 titanSheet.values
    |> List.map (\row ->
      AllianceTitanScore
        ( getAt fixedTitanIndexes.dateIndex row |> withDefault "NO DATE" )
        ( getIntFromRow row fixedTitanIndexes.totalIndex 0 )
        ( getTitanColorFromRow row fixedTitanIndexes.colorIndex )
        ( getIntFromRow row fixedTitanIndexes.starIndex 0 )
      )

extractWarAllianceScores : RawSheet -> List AllianceWarScore
extractWarAllianceScores warSheet =
  List.drop 1 warSheet.values
    |> List.map (\row ->
      AllianceWarScore
        ( getAt fixedWarIndexes.dateIndex row |> withDefault "NO DATE" )
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
    MemberTitanScore
      ( getAt fixedTitanIndexes.dateIndex row |> withDefault "NO DATE" )
      memberScore
      titanColor
      titanStars

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
    MemberWarScore
      ( getAt fixedWarIndexes.dateIndex row |> withDefault "NO DATE" )
      memberScore
      warBonus

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
      ( extractTitanAllianceScores titanSheet )
      ( extractWarAllianceScores warSheet )
      ( extractTitanMemberScores titanSheet )
      ( extractWarMemberScores warSheet )

allianceTitanColorPredicate : StatsFilterExtender r -> AllianceTitanScore -> Bool
allianceTitanColorPredicate { filteredTitanColor } { titanColor } = filteredTitanColor == allTitanColors || titanColor == filteredTitanColor

allianceTitanStarsPredicate : StatsFilterExtender r -> AllianceTitanScore -> Bool
allianceTitanStarsPredicate { filteredTitanStars } { titanStars } =
  case filteredTitanStars of
    Nothing -> True
    Just stars -> titanStars == stars

filterAllianceTitanScores : StatsFilterExtender r -> List AllianceTitanScore -> FilteredAllianceTitanScores
filterAllianceTitanScores statsFilter allianceTitanScores =
  let
    filteredAllianceScores : List AllianceTitanScore
    filteredAllianceScores = List.filter ( allianceTitanColorPredicate statsFilter ) allianceTitanScores
      |> List.filter ( allianceTitanStarsPredicate statsFilter )
      |> takeLast statsFilter.filteredTitanPeriod

    preferredTitanColor : DetailedColor
    preferredTitanColor = findPreferredEventType ( .name << .titanColor ) ( Just << .damage ) filteredAllianceScores
      |> withDefault ""
      |> titanColorFromString
  in
    { averageTitanScore = computeAverageDamage filteredAllianceScores
    , maxTitanScore = List.map .damage filteredAllianceScores |> List.maximum |> Maybe.withDefault 0
    , progression = List.map .damage filteredAllianceScores
        |> List.indexedMap (\index damage -> [index, damage])
        |> computeLinearRegression 2
    , preferredTitanColor = preferredTitanColor
    , titanScores = filteredAllianceScores
    }

allianceWarBonusPredicate : StatsFilterExtender r -> AllianceWarScore -> Bool
allianceWarBonusPredicate { filteredWarBonus } { warBonus } =
  case filteredWarBonus of
    Nothing -> True
    Just bonus -> bonus == warBonus

filterAllianceWarScores : StatsFilterExtender r -> List AllianceWarScore -> FilteredAllianceWarScores
filterAllianceWarScores statsFilter allianceWarScores =
  let
    filteredAllianceScores : List AllianceWarScore
    filteredAllianceScores = List.filter ( allianceWarBonusPredicate statsFilter ) allianceWarScores
      |> takeLast statsFilter.filteredWarPeriod

    preferredWarBonus : String
    preferredWarBonus = findPreferredEventType ( .warBonus ) ( Just << .damage ) filteredAllianceScores
      |> withDefault ""
      |> sanitizeExternalWarBonus
  in
    { averageWarScore = computeAverageDamage filteredAllianceScores
    , maxWarScore = List.map .damage filteredAllianceScores |> List.maximum |> Maybe.withDefault 0
    , preferredWarBonus = preferredWarBonus
    , warScores = filteredAllianceScores
    }

memberTitanColorPredicate : StatsFilterExtender r -> MemberTitanScore -> Bool
memberTitanColorPredicate { filteredTitanColor } { titanColor } = filteredTitanColor == allTitanColors || titanColor == filteredTitanColor

memberTitanStarsPredicate : StatsFilterExtender r -> MemberTitanScore -> Bool
memberTitanStarsPredicate { filteredTitanStars } { titanStars } =
  case filteredTitanStars of
    Nothing -> True
    Just stars -> titanStars == stars

filterMemberTitanScores : StatsFilterExtender r -> MemberTitanScores -> FilteredMemberTitanScores
filterMemberTitanScores statsFilter memberTitanScores =
  let
    filteredMemberScores : List MemberTitanScore
    filteredMemberScores = List.filter ( memberTitanColorPredicate statsFilter ) memberTitanScores.titanScores
      |> List.filter ( memberTitanStarsPredicate statsFilter )
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
    , progression = List.map .score filteredMemberScores
      |> List.filter hasValue
      |> List.map ( Maybe.map .damage )
      |> List.map ( Maybe.withDefault 0 ) -- Default value not reachable, cf filter
      |> List.indexedMap (\index damage -> [index, damage] )
      |> computeLinearRegression 2
    , preferredTitanColor = preferredTitanColor
    , titanScores = filteredMemberScores
    }

memberWarBonusPredicate : StatsFilterExtender r -> MemberWarScore -> Bool
memberWarBonusPredicate { filteredWarBonus } { warBonus } =
  case filteredWarBonus of
    Nothing -> True
    Just bonus -> bonus == warBonus

filterMemberWarScores : StatsFilterExtender r -> MemberWarScores -> FilteredMemberWarScores
filterMemberWarScores statsFilter memberWarScores =
  let
    filteredMemberScores : List MemberWarScore
    filteredMemberScores = List.filter ( memberWarBonusPredicate statsFilter ) memberWarScores.warScores
      |> takeLast statsFilter.filteredTitanPeriod

    preferredWarBonus : Maybe String
    preferredWarBonus = findPreferredEventType ( .warBonus ) ( ( Maybe.map .damage ) << .score ) filteredMemberScores
      |> Maybe.map sanitizeExternalWarBonus
  in
    { pseudo = memberWarScores.pseudo
    , averageScore = List.map .score filteredMemberScores |> computeAverageScore
    , maxScore = List.map .score filteredMemberScores
      |> List.filter hasValue
      |> List.map ( Maybe.map .damage )
      |> List.map ( Maybe.withDefault 0 ) -- Default value not reachable, cf filter
      |> List.maximum
    , preferredWarBonus = preferredWarBonus
    , warScores = filteredMemberScores
    }

filterStats : StatsFilterExtender r -> Stats -> FilteredStats
filterStats statsFilter stats =
  { allianceTitanScores = filterAllianceTitanScores statsFilter stats.allianceTitanScores
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
  ( FilteredMemberWarScores "" ( AverageMemberScore False 0 0 ) Nothing Nothing [] ) -- Can't fail anyway, checked before that
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
  , maxWarScore = filteredStats.allianceWarScores.maxWarScore
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

