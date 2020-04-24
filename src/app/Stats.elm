module Stats exposing (
  Stats, AllianceStats, MemberStats, FilteredStats, StatsExtender,
  fetchAllStats,
  updateStats, updateStatsWithFilter,
  viewAllianceStats, viewTitansStats, viewWarsStats
  )

import AppConfig exposing (AppConfigExtender)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Maybe exposing (..)

import Msg exposing (..)
import Flip exposing (..)
import GetAt exposing (..)
import TakeLast exposing (..)
import Spinner exposing (..)
import MaybeExtra exposing (..)
import CustomStyle exposing (..)
import PresentDate exposing (..)
import Gsheet exposing (..)
import PresentNumber exposing (..)
import AreListsEqual exposing (..)
import Translations exposing (..)
import ComputeTeamValue exposing (..)
import CreateBearerHeader exposing (..)
import MemberScore exposing (..)
import MapWithPreviousAndNext exposing (..)
import FindPreferredEventType exposing (..)
import Wars exposing (..)
import ComputeAverage exposing (..)
import LinearRegression exposing (..)
import Titans exposing (..)
import StatsFilter exposing (..)
import Gsheet exposing (..)

------------
-- MODELS --
------------

type alias Model r = AppConfigExtender (StatsFilterExtender (StatsExtender (TranslationsExtender r)))

type alias StatsExtender r =
  { r
  | isAdmin: Bool
  , stats: Maybe Stats
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
  , preferredTitanColor : TitanColor
  , averageWarScore : Float
  , maxWarScore : Int
  , preferredWarBonus : WarBonus
  , memberStats : Dict String MemberStats
  }

type alias MemberStats =
  { pseudo : String
  , averageTitanScore : AverageMemberScore
  , maxTitanScore : Maybe Int
  , preferredTitanColor : Maybe TitanColor
  , averageWarScore : AverageMemberScore
  , preferredWarBonus : Maybe WarBonus
  , titanTeamValue : Float
  , warTeamValue : Float
  , teamValue : Float
  }

type alias AllianceTitanScore =
  { date : String
  , damage : Int
  , titanColor : TitanColor
  , titanStars : Int
  }

type alias AllianceWarScore =
  { date : String
  , damage : Int
  , warBonus : WarBonus
  }

type alias MemberTitanScores =
  { pseudo : String
  , titanScores : List MemberTitanScore
  }

type alias MemberTitanScore =
  { date : String
  , score : MemberScore
  , titanColor : TitanColor
  , titanStars : Int
  }

type alias MemberWarScores =
  { pseudo : String
  , warScores : List MemberWarScore
  }

type alias MemberWarScore =
  { date : String
  , score : MemberScore
  , warBonus : WarBonus
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
  , preferredTitanColor : TitanColor
  , titanScores : List AllianceTitanScore
  }

type alias FilteredAllianceWarScores =
  { averageWarScore : Float
  , maxWarScore : Int
  , progression : RegressionResult
  , preferredWarBonus : WarBonus
  , warScores : List AllianceWarScore
  }

type alias FilteredMemberTitanScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , maxScore : Maybe Int
  , progression : RegressionResult
  , preferredTitanColor : Maybe TitanColor
  , titanScores : List MemberTitanScore
  }

type alias FilteredMemberWarScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , maxScore : Maybe Int
  , progression : RegressionResult
  , preferredWarBonus : Maybe WarBonus
  , warScores : List MemberWarScore
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

viewAllianceStats : Model r -> Html Msg
viewAllianceStats model = case model.statsError of
  Just errorMessage -> viewErrorMessage model errorMessage
  Nothing ->
    case model.allianceStats of
      Just validAllianceStats -> viewValidAllianceStats model validAllianceStats
      Nothing -> viewSpinner model.translations.fetchingTheData

viewTitansStats : Model r -> Html Msg
viewTitansStats model = case model.statsError of
  Just errorMessage -> viewErrorMessage model errorMessage
  Nothing ->
    case model.filteredStats of
      Just validFilteredStats -> viewValidTitansStats model validFilteredStats
      Nothing -> viewSpinner model.translations.fetchingTheData

viewWarsStats : Model r -> Html Msg
viewWarsStats model = case model.statsError of
  Just errorMessage -> viewErrorMessage model errorMessage
  Nothing ->
    case model.filteredStats of
      Just validFilteredStats -> viewValidWarsStats model validFilteredStats
      Nothing -> viewSpinner model.translations.fetchingTheData

viewErrorMessage : Model r -> String -> Html Msg
viewErrorMessage { translations } errorMessage =
  div [ class "error-page" ] [
    div [ class "error-message" ] [ text errorMessage ],
    div [ class "button button-primary", role "button", onClick ( StatsMsg BackToAppKeyMsg ) ] [ text translations.backToAppKeyPage ]
  ]

compareMembersStats : MemberStats -> MemberStats -> Order
compareMembersStats stats1 stats2 =
  if stats1.teamValue > stats2.teamValue then GT
  else if stats1.teamValue < stats2.teamValue then LT
  else compare stats1.pseudo stats2.pseudo

computeTendencyAttributes : Int -> List MemberScore -> List (Attribute msg)
computeTendencyAttributes maxDamage scores =
  let
    predict : Int -> Float
    predict = List.map .damage scores
      |> List.indexedMap (\index damage -> [index, damage])
      |> computeLinearRegression 2
      |> .predict

    startRatio : Float
    startRatio = predict 0 |> flip (/) (toFloat maxDamage)

    endRatio : Float
    endRatio = predict (List.length scores)
      |> flip (/) (toFloat maxDamage)

    slope : Float
    slope = computeSlope startRatio endRatio / (toFloat (List.length scores))

    slopeClass : String
    slopeClass = if slope == 0 then "tendency-neutral"
      else if slope < 0 then "tendency-negative"
      else "tendency-positive"

    offsetRatio : Float
    offsetRatio = computeOffsetRatio slope
  in
    [
      class ("line tendency " ++ slopeClass),
      customStyle [
              ("--start-percent-to-max", startRatio |> percentAsCss),
              ("--end-percent-to-max", endRatio |> percentAsCss),
              ("--offset-ratio", String.fromFloat offsetRatio)
            ]
    ]

viewValidTitansStats : Model r -> FilteredStats -> Html Msg
viewValidTitansStats model filteredStats =
  let
    genericTitanScores : FilteredMemberTitanScores
    genericTitanScores = List.filter (\memberScores -> model.filteredMember == memberScores.pseudo) filteredStats.membersTitanScores
      |> List.head
      |> withDefault (generifyAllianceTitanScores model.translations.alliance filteredStats.allianceTitanScores)

    maxDamage : Int
    maxDamage = withDefault 0 genericTitanScores.maxScore

    eventsNumberStyle : Attribute msg
    eventsNumberStyle = customStyle [
        ( "--events-number", List.length filteredStats.allianceTitanScores.titanScores |> String.fromInt )
      ]

  in
    div [ class "stats" ] [
      viewTitansFilterForm model ( model.translations.alliance :: ( List.map .pseudo filteredStats.membersTitanScores ) ),
      div [ class "titan-stats" ] [
        div [ class "graphs-container" ] [
          h2 [ class "chart-title" ] [ text model.translations.titanScores ],
          div [ class "progression" ] [ viewGradient genericTitanScores.progression.gradient ],
          div [ class "graph-container", ariaHidden True, eventsNumberStyle ] [
            div [ class "chart"] [
              div [ class "label max-label" ] [
                span [ class "label-value" ] [ withDefault 0 genericTitanScores.maxScore |> presentNumber |> text ]
              ],
              div [ class "label zero-label" ] [
                span [ class "label-value" ] [ text "0" ]
              ],
              div [ class "member-stats" ] (
                viewGenericTitanScores model.translations genericTitanScores ++ [
                  div (List.map .score genericTitanScores.titanScores |> computeTendencyAttributes maxDamage) []
                ]
              )
            ]
          ]
        ]
      ]
    ]

viewValidWarsStats : Model r -> FilteredStats -> Html Msg
viewValidWarsStats model filteredStats =
  let
    genericWarScores : FilteredMemberWarScores
    genericWarScores = List.filter (\memberScores -> model.filteredMember == memberScores.pseudo) filteredStats.membersWarScores
      |> List.head
      |> withDefault (generifyAllianceWarScores model.translations.alliance filteredStats.allianceWarScores)

    maxDamage : Int
    maxDamage = withDefault 0 genericWarScores.maxScore

    eventsNumberStyle : Attribute msg
    eventsNumberStyle = customStyle [
        ( "--events-number", List.length filteredStats.allianceWarScores.warScores |> String.fromInt )
      ]

  in
    div [ class "stats" ] [
      viewWarsFilterForm model ( model.translations.alliance :: ( List.map .pseudo filteredStats.membersTitanScores ) ),
      div [ class "war-stats" ] [
        div [ class "graphs-container" ] [
          h2 [ class "chart-title" ] [ text model.translations.warScores ],
          div [ class "progression" ] [ viewGradient genericWarScores.progression.gradient ],
          div [ class "graph-container", ariaHidden True, eventsNumberStyle ] [
            div [ class "chart"] [
              div [ class "label max-label" ] [
                span [ class "label-value" ] [ withDefault 0 genericWarScores.maxScore |> presentNumber |> text ]
              ],
              div [ class "label zero-label" ] [
                span [ class "label-value" ] [ text "0" ]
              ],
              div [ class "member-stats" ] (
                viewGenericWarScores model.translations genericWarScores ++ [
                  div (List.map .score genericWarScores.warScores |> computeTendencyAttributes maxDamage) []
                ]
              )
            ]
          ]
        ]
      ]
    ]

viewGradient : Float -> Html msg
viewGradient gradient =
  let
    sign : String
    sign = if  gradient == 0 then ""
      else if gradient > 0 then "+"
      else "-"

    className : String
    className = if gradient == 0 then "progression-neutral"
      else if gradient < 0 then "progression-negative"
      else "progression-positive"

    value : String
    value = round gradient |> abs |> String.fromInt
  in
    p [] [
      span [ class "progression-label" ] [ text "Progression" ],
      span [ class className ] [ text ( sign ++ value ) ],
      span [ class className ] [ text "points/event" ]
    ]

generifyAllianceTitanScores : String -> FilteredAllianceTitanScores -> FilteredMemberTitanScores
generifyAllianceTitanScores allianceName allianceTitanScores =
  { pseudo = allianceName
  , averageScore = AverageMemberScore allianceTitanScores.averageTitanScore 0
  , maxScore = Just allianceTitanScores.maxTitanScore
  , progression = allianceTitanScores.progression
  , preferredTitanColor = Just allianceTitanScores.preferredTitanColor
  , titanScores = List.map generifyAllianceTitanScore allianceTitanScores.titanScores
  }

generifyAllianceTitanScore : AllianceTitanScore -> MemberTitanScore
generifyAllianceTitanScore allianceTitanScore =
  { date = allianceTitanScore.date
  , score = MemberScore allianceTitanScore.damage 0
  , titanColor = allianceTitanScore.titanColor
  , titanStars = allianceTitanScore.titanStars
  }

generifyAllianceWarScores : String -> FilteredAllianceWarScores -> FilteredMemberWarScores
generifyAllianceWarScores allianceName allianceWarScores =
  { pseudo = allianceName
  , averageScore = AverageMemberScore allianceWarScores.averageWarScore 0
  , maxScore = Just allianceWarScores.maxWarScore
  , progression = allianceWarScores.progression
  , preferredWarBonus = Just allianceWarScores.preferredWarBonus
  , warScores = List.map generifyAllianceWarScore allianceWarScores.warScores
  }

generifyAllianceWarScore : AllianceWarScore -> MemberWarScore
generifyAllianceWarScore allianceWarScore =
  { date = allianceWarScore.date
  , score = MemberScore allianceWarScore.damage 0
  , warBonus = allianceWarScore.warBonus
  }

viewGenericTitanScores : Translations -> FilteredMemberTitanScores -> List (Html Msg)
viewGenericTitanScores translations scores = mapWithPreviousAndNext
  ( viewGenericTitanScore translations ( withDefault 0 scores.maxScore ) )
  scores.titanScores

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

computeIncomingLineData : Maybe { r | score: MemberScore } -> Float -> Int -> LineData msg
computeIncomingLineData maybePreviousScore currentPercent maxDamage =
  case maybePreviousScore of
    Nothing -> emptyLineData
    Just previousScore ->
      let
        previousDamage : Int
        previousDamage = previousScore.score.damage

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

computeOutgoingLineData : Maybe { r | score: MemberScore } -> Float -> Int -> LineData msg
computeOutgoingLineData maybeNextScore currentPercent maxDamage =
  case maybeNextScore of
    Nothing -> emptyLineData
    Just nextScore ->
      let
        nextDamage : Int
        nextDamage = nextScore.score.damage

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

viewTitanColor : TitanColorData -> Html Msg
viewTitanColor { code, name, icon } =
  div [ class ("bullet-point titan-color-" ++ (code |> String.toLower)), title name ] [
    i [ class icon ] []
  ]

viewGenericTitanScore : Translations -> Int -> (Maybe MemberTitanScore, MemberTitanScore, Maybe MemberTitanScore) -> Html Msg
viewGenericTitanScore translations maxDamage (maybePreviousScore, currentScore, maybeNextScore) =
  let
    currentDamage : Int
    currentDamage = currentScore.score.damage

    titanColorData : TitanColorData
    titanColorData = getTitanColorData translations currentScore.titanColor

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
        viewTitanColor titanColorData,
        div [ class "tooltip-trigger" ] [
          p [ class "tooltip" ] [
            span [] [
              text ( String.join " " [ titanColorData.name, String.fromInt currentScore.titanStars ] ),
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

viewWarBonus : WarBonusData -> Html Msg
viewWarBonus { code, name, icon } =
  div [ class ("bullet-point war-bonus war-bonus-" ++ (code |> String.toLower)), title name ] [
    i [ class icon ] []
  ]

viewGenericWarScore : Translations -> Int -> (Maybe MemberWarScore, MemberWarScore, Maybe MemberWarScore) -> Html Msg
viewGenericWarScore translations maxDamage (maybePreviousScore, currentScore, maybeNextScore) =
  let
    currentDamage : Int
    currentDamage = currentScore.score.damage

    warBonusName : String
    warBonusName = getWarBonusData translations currentScore.warBonus |> .name

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
        viewWarBonus ( getWarBonusData translations currentScore.warBonus ),
        div [ class "tooltip-trigger" ] [
          p [ class "tooltip" ] [
            span [] [ text warBonusName ],
            br [] [],
            span [] [ presentNumber currentDamage |> text ],
            br [] [],
            span [] [ presentDate currentScore.date |> text ]
          ]
        ]
      ],
      div [ class outgoingLineData.lineType, outgoingLineData.lineStyle ] []
    ]

viewGenericWarScores : Translations -> FilteredMemberWarScores -> List (Html Msg)
viewGenericWarScores translations scores = mapWithPreviousAndNext
  ( viewGenericWarScore translations ( withDefault 0 scores.maxScore ) )
  scores.warScores

viewValidAllianceStats : Model r -> AllianceStats -> Html Msg
viewValidAllianceStats { translations } allianceStats =
  div [ class "alliance" ] [
    div [ class "alliance-members" ] [
      h2 [] [ text translations.alliance ],
      div [ class "alliance-stats" ] [
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text translations.averageTitanScore ],
          span [ class "alliance-stat-value" ] [ text <| presentNumber <| round <| allianceStats.averageTitanScore ]
        ],
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text translations.preferredTitanColor ],
          span [ class "alliance-stat-value" ] [ viewTitanColor ( getTitanColorData translations allianceStats.preferredTitanColor )  ]
        ],
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text translations.averageWarScore ],
          span [ class "alliance-stat-value" ] [ text <| presentNumber <| round <| allianceStats.averageWarScore ]
        ],
        div [ class "alliance-stat" ] [
          span [ class "alliance-stat-name" ] [ text translations.preferredWarBonus ],
          span [ class "alliance-stat-value" ] [ viewWarBonus ( getWarBonusData translations allianceStats.preferredWarBonus ) ]
        ]
      ],
      h2 [] [ text translations.allianceMembers ],
      table [] [
        thead [] [
          th [] [ text translations.pseudo ],
          th [] [ text translations.averageTitanScore ],
          th [] [ text translations.preferredTitanColor ],
          th [] [ text translations.averageWarScore ],
          th [] [ text translations.preferredWarBonus ],
          th [] [ text translations.teamValue ]
        ],
        tbody [] (
          Dict.values allianceStats.memberStats
            |> List.sortWith compareMembersStats
            |> List.reverse
            |> List.map ( viewMember translations )
        )
      ]
    ]
  ]

teamValuesTitle : Translations -> { r | titanTeamValue: Float, warTeamValue: Float } -> String
teamValuesTitle translations { titanTeamValue, warTeamValue } =
  String.join "" [
    translations.titans, ": ", presentNumber <| round titanTeamValue,
    "\n",
    translations.wars, ": ", presentNumber <| round warTeamValue
  ]

viewMember : Translations -> MemberStats -> Html Msg
viewMember translations memberStats =
  tr [ class "member-row" ] [
    th [ class "member-pseudo" ] [ text memberStats.pseudo ],
    td
      [ class "member-value" ]
      [ text <| presentNumber <| round memberStats.averageTitanScore.damage ],
    td [ class "member-value" ] [ viewTitanColor <| ( getTitanColorData translations ) <| withDefault ALL memberStats.preferredTitanColor ],
    td
      [ class "member-value" ]
      [ text <| presentNumber <| round memberStats.averageWarScore.damage ],
    td [ class "member-value" ] [ viewWarBonus <| ( getWarBonusData translations ) <| withDefault AllBonus memberStats.preferredWarBonus ],
    td [ class "member-value"
       , title ( teamValuesTitle translations memberStats )
       ] [ text <| presentNumber <| round memberStats.teamValue ]
  ]

------------
-- UPDATE --
------------

computeScore : Int -> Int -> Int -> MemberScore
computeScore damage allianceScore membersNumber =
  MemberScore damage ( computeTeamValue allianceScore membersNumber damage )

getIntFromRow : List String -> Int -> Int -> Int
getIntFromRow row index default = getAt index row |> Maybe.andThen String.toInt |> withDefault default

getTitanColorFromRow : List String -> Int -> TitanColor
getTitanColorFromRow row index = getAt index row |> withDefault "" |> titanColorFromString

getWarBonusFromRow : List String -> Int -> WarBonus
getWarBonusFromRow row index = getAt index row |> withDefault "" |> warBonusFromString

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
    allianceScore : Int
    allianceScore = getIntFromRow row fixedTitanIndexes.totalIndex 0

    membersNumber : Int
    membersNumber = getIntFromRow row fixedTitanIndexes.membersIndex 0

    memberDamage : Int
    memberDamage = getIntFromRow row memberIndex 0

    memberScore : MemberScore
    memberScore = computeScore memberDamage allianceScore membersNumber

    titanColor : TitanColor
    titanColor = getTitanColorFromRow row fixedTitanIndexes.colorIndex

    titanStars : Int
    titanStars = getIntFromRow row fixedTitanIndexes.starIndex 0
  in
    MemberTitanScore
      ( getAt fixedTitanIndexes.dateIndex row |> withDefault "NO DATE" )
      memberScore
      titanColor
      titanStars

hasScore : Int -> List String -> Bool
hasScore index data =
  getAt index data
    |> Maybe.andThen String.toInt
    |> hasValue

extractTitanDataForMember : List ( List String ) -> Int -> Int -> String -> MemberTitanScores
extractTitanDataForMember data offset index memberPseudo =
  let
    memberDataIndex : Int
    memberDataIndex = offset + index

    memberScores : List MemberTitanScore
    memberScores = List.filter ( hasScore memberDataIndex ) data
      |> List.map ( extractMemberTitanScore memberDataIndex )

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
    allianceScore : Int
    allianceScore = getIntFromRow row fixedWarIndexes.totalIndex 0

    membersNumber : Int
    membersNumber = getIntFromRow row fixedWarIndexes.membersIndex 0

    memberDamage : Int
    memberDamage = getIntFromRow row memberIndex 0

    memberScore : MemberScore
    memberScore = computeScore memberDamage allianceScore membersNumber

    warBonus : WarBonus
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
    memberScores = List.filter ( hasScore memberDataIndex ) data
     |> List.map ( extractMemberWarScore memberDataIndex )

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

titanColorPredicate : StatsFilterExtender r -> { u | titanColor : TitanColor } -> Bool
titanColorPredicate { filteredTitanColor } { titanColor } = filteredTitanColor == ALL || titanColor == filteredTitanColor

titanStarsPredicate : StatsFilterExtender r -> { u | titanStars : Int } -> Bool
titanStarsPredicate { filteredTitanStars } { titanStars } =
  case filteredTitanStars of
    Nothing -> True
    Just stars -> titanStars == stars

filterAllianceTitanScores : StatsFilterExtender r -> List AllianceTitanScore -> FilteredAllianceTitanScores
filterAllianceTitanScores statsFilter allianceTitanScores =
  let
    filteredAllianceScores : List AllianceTitanScore
    filteredAllianceScores = List.filter ( titanColorPredicate statsFilter ) allianceTitanScores
      |> List.filter ( titanStarsPredicate statsFilter )
      |> takeLast statsFilter.filteredTitanPeriod

    preferredTitanColor : TitanColor
    preferredTitanColor = findPreferredEventType2 .titanColor .damage filteredAllianceScores |> withDefault ALL

  in
    { averageTitanScore = computeAverageDamage filteredAllianceScores
    , maxTitanScore = List.map .damage filteredAllianceScores |> List.maximum |> Maybe.withDefault 0
    , progression = List.map .damage filteredAllianceScores
        |> List.indexedMap (\index damage -> [index, damage])
        |> computeLinearRegression 2
    , preferredTitanColor = preferredTitanColor
    , titanScores = filteredAllianceScores
    }

warBonusPredicate : StatsFilterExtender r -> { u | warBonus : WarBonus } -> Bool
warBonusPredicate { filteredWarBonus } { warBonus } = filteredWarBonus == AllBonus || warBonus == filteredWarBonus

filterAllianceWarScores : StatsFilterExtender r -> List AllianceWarScore -> FilteredAllianceWarScores
filterAllianceWarScores statsFilter allianceWarScores =
  let
    filteredAllianceScores : List AllianceWarScore
    filteredAllianceScores = List.filter ( warBonusPredicate statsFilter ) allianceWarScores
      |> takeLast statsFilter.filteredWarPeriod

    preferredWarBonus : WarBonus
    preferredWarBonus = findPreferredEventType2 .warBonus .damage filteredAllianceScores |> withDefault AllBonus

  in
    { averageWarScore = computeAverageDamage filteredAllianceScores
    , maxWarScore = List.map .damage filteredAllianceScores |> List.maximum |> Maybe.withDefault 0
    , progression = List.map .damage filteredAllianceScores
        |> List.indexedMap (\index damage -> [index, damage])
        |> computeLinearRegression 2
    , preferredWarBonus = preferredWarBonus
    , warScores = filteredAllianceScores
    }

filterMemberTitanScores : StatsFilterExtender r -> MemberTitanScores -> FilteredMemberTitanScores
filterMemberTitanScores statsFilter memberTitanScores =
  let
    filteredMemberScores : List MemberTitanScore
    filteredMemberScores = List.filter ( titanColorPredicate statsFilter ) memberTitanScores.titanScores
      |> List.filter ( titanStarsPredicate statsFilter )
      |> takeLast statsFilter.filteredTitanPeriod

    preferredTitanColor : Maybe TitanColor
    preferredTitanColor = findPreferredEventType2 .titanColor ( .damage << .score ) filteredMemberScores

  in
    { pseudo = memberTitanScores.pseudo
    , averageScore = List.map .score filteredMemberScores |> computeAverageScore
    , maxScore = List.map .score filteredMemberScores
      |> List.map .damage
      |> List.maximum
    , progression = List.map .score filteredMemberScores
      |> List.map .damage
      |> List.indexedMap (\index damage -> [index, damage] )
      |> computeLinearRegression 2
    , preferredTitanColor = preferredTitanColor
    , titanScores = filteredMemberScores
    }

filterMemberWarScores : StatsFilterExtender r -> MemberWarScores -> FilteredMemberWarScores
filterMemberWarScores statsFilter memberWarScores =
  let
    filteredMemberScores : List MemberWarScore
    filteredMemberScores = List.filter ( warBonusPredicate statsFilter ) memberWarScores.warScores
      |> takeLast statsFilter.filteredWarPeriod

    preferredWarBonus : Maybe WarBonus
    preferredWarBonus = findPreferredEventType2 .warBonus ( .damage << .score ) filteredMemberScores

  in
    { pseudo = memberWarScores.pseudo
    , averageScore = List.map .score filteredMemberScores |> computeAverageScore
    , maxScore = List.map .score filteredMemberScores
      |> List.map .damage
      |> List.maximum
    , progression = List.map .score filteredMemberScores
      |> List.map .damage
      |> List.indexedMap (\index damage -> [index, damage] )
      |> computeLinearRegression 2
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

filterStatsForAlliancePage : Stats -> StatsFilterExtender r -> FilteredStats
filterStatsForAlliancePage stats defaultStatsFilter = filterStats defaultStatsFilter stats

decodeStats : String -> RawStats
decodeStats statsAsString = decodeRawStats statsAsString

retrieveMemberWarScores : Int -> List ( FilteredMemberWarScores ) -> FilteredMemberWarScores
retrieveMemberWarScores index memberWarScoresList = Maybe.withDefault
  ( FilteredMemberWarScores "" ( AverageMemberScore 0 0 ) Nothing (RegressionResult 0 0 (\_ -> 0)) Nothing [] ) -- Can't fail anyway, checked before that
  ( getAt index memberWarScoresList )

mergeTeamValues : AverageMemberScore -> AverageMemberScore -> Float
mergeTeamValues titanAverageScore warAverageScore = ( titanAverageScore.teamValue + warAverageScore.teamValue ) / 2

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
    , titanTeamValue = memberTitanScores.averageScore.teamValue
    , warTeamValue = memberWarScores.averageScore.teamValue
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

updateValidStats : Stats -> Model r -> Model r
updateValidStats stats model =
  let
    defaultStatsFilter : StatsFilter
    defaultStatsFilter = createDefaultStatsFilter model.translations

    filteredStats : FilteredStats
    filteredStats = filterStatsForAlliancePage stats defaultStatsFilter

    allianceStats : AllianceStats
    allianceStats = computeAllianceStats filteredStats
  in
    { model
    | stats = Just stats
    , allianceStats = Just allianceStats
    , filteredStats = Just filteredStats
    }

decodeAndUpdateStats : String -> Model r -> Cmd Msg -> ( Model r, Cmd Msg )
decodeAndUpdateStats statsAsString model fetchRightsCmd =
  let
    rawStats : RawStats
    rawStats = decodeStats statsAsString

    stats : Stats
    stats = parseRawStats rawStats

    areMembersListEqual : Bool
    areMembersListEqual = areListsEqual
      ( List.map .pseudo stats.membersTitanScores )
      ( List.map .pseudo stats.membersWarScores )

    maybeErrorMessage : Maybe String
    maybeErrorMessage = if areMembersListEqual then Nothing else Just model.translations.membersListsDiffer

  in
    case maybeErrorMessage of
      Nothing -> ( updateValidStats stats model, fetchRightsCmd )
      Just _ -> ( { model | statsError = maybeErrorMessage }, Cmd.none )

updateStats : String -> Model r -> Cmd Msg -> ( Model r, Cmd Msg )
updateStats statsAsString model fetchRightsCmd = decodeAndUpdateStats statsAsString model fetchRightsCmd

updateStatsWithFilter : Model r -> Model r
updateStatsWithFilter model =
  { model
  | filteredStats = Maybe.map ( filterStats model ) model.stats
  }

