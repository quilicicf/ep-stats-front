module Stats exposing (
  Stats, AllianceStats, MemberStats, AnalyzedStats, StatsExtender,
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

goodParticipation : Float
goodParticipation = 0.9

acceptableParticipation : Float
acceptableParticipation = 0.8

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
  , filteredStats: Maybe AnalyzedStats
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
  , titanParticipation : Float
  , preferredTitanColor : Maybe TitanColor
  , averageWarScore : AverageMemberScore
  , warParticipation : Float
  , preferredWarBonus : Maybe WarBonus
  , participation : Float
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

type alias AnalyzedStats =
  { allianceTitanScores : AnalyzedAllianceTitanScores
  , allianceWarScores: AnalyzedAllianceWarScores
  , membersTitanScores: List AnalyzedMemberTitanScores
  , membersWarScores: List AnalyzedMemberWarScores
  }

type alias AnalyzedAllianceTitanScores =
  { averageTitanScore : Float
  , maxTitanScore : Int
  , progression : RegressionResult
  , preferredTitanColor : TitanColor
  , titanScores : List AllianceTitanScore
  }

type alias AnalyzedAllianceWarScores =
  { averageWarScore : Float
  , maxWarScore : Int
  , progression : RegressionResult
  , preferredWarBonus : WarBonus
  , warScores : List AllianceWarScore
  }

type alias AnalyzedMemberTitanScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , maxScore : Maybe Int
  , participation : Float
  , progression : RegressionResult
  , preferredTitanColor : Maybe TitanColor
  , titanScores : List MemberTitanScore
  }

type alias AnalyzedMemberWarScores =
  { pseudo : String
  , averageScore : AverageMemberScore
  , maxScore : Maybe Int
  , participation : Float
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

getParticipationPercentage : List MemberScore -> Float
getParticipationPercentage memberScores =
  let
    allScoresNumber : Int
    allScoresNumber = List.length memberScores

    hittingScoresNumber : Int
    hittingScoresNumber = List.filter (\memberScore -> memberScore.damage /= 0) memberScores |> List.length
  in
    (toFloat hittingScoresNumber) / (toFloat allScoresNumber)

viewValidTitansStats : Model r -> AnalyzedStats -> Html Msg
viewValidTitansStats model filteredStats =
  let
    genericTitanScores : AnalyzedMemberTitanScores
    genericTitanScores = List.filter (\memberScores -> model.filteredMember == memberScores.pseudo) filteredStats.membersTitanScores
      |> List.head
      |> withDefault (generifyAllianceTitanScores model.translations.alliance filteredStats.allianceTitanScores)

    participationPercentage : Float
    participationPercentage = List.map .score genericTitanScores.titanScores |> getParticipationPercentage

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
          div [ class "progression" ] [
            span [ class "progression-label" ] [ text model.translations.progression ],
            viewGradient model.translations genericTitanScores.progression.gradient
          ],
          div [ class "participation" ] [
            span [ class "participation-label" ] [ text model.translations.participation ],
            viewParticipation participationPercentage
          ],
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

viewValidWarsStats : Model r -> AnalyzedStats -> Html Msg
viewValidWarsStats model filteredStats =
  let
    genericWarScores : AnalyzedMemberWarScores
    genericWarScores = List.filter (\memberScores -> model.filteredMember == memberScores.pseudo) filteredStats.membersWarScores
      |> List.head
      |> withDefault (generifyAllianceWarScores model.translations.alliance filteredStats.allianceWarScores)

    participationPercentage : Float
    participationPercentage = List.map .score genericWarScores.warScores |> getParticipationPercentage

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
          div [ class "progression" ] [
            span [ class "progression-label" ] [ text model.translations.progression ],
            viewGradient model.translations genericWarScores.progression.gradient
          ],
          div [ class "participation" ] [
            span [ class "participation-label" ] [ text model.translations.participation ],
            viewParticipation participationPercentage
          ],
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

viewGradient : Translations -> Float -> Html msg
viewGradient translations gradient =
  let
    sign : String
    sign = if  gradient == 0 then ""
      else if gradient > 0 then "+"
      else "-"

    className : String
    className = if gradient == 0 then "neutral"
      else if gradient < 0 then "negative"
      else "positive"

    value : String
    value = round gradient |> abs |> presentNumber
  in
    span [ class className ] [ sign ++ value ++ translations.pointsPerEvent |> text ]

viewParticipation : Float -> Html msg
viewParticipation participation =
  let
    displayablePercentage : String
    displayablePercentage = participation * 100 |> round |> String.fromInt

    className : String
    className = if participation >= goodParticipation then "positive"
      else if participation >= acceptableParticipation then "neutral"
      else "negative"
  in
    span [ class className ] [ displayablePercentage ++ "%" |> text ]

generifyAllianceTitanScores : String -> AnalyzedAllianceTitanScores -> AnalyzedMemberTitanScores
generifyAllianceTitanScores allianceName allianceTitanScores =
  { pseudo = allianceName
  , averageScore = AverageMemberScore allianceTitanScores.averageTitanScore 0
  , maxScore = Just allianceTitanScores.maxTitanScore
  , participation = 100 -- The alliance always participates
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

generifyAllianceWarScores : String -> AnalyzedAllianceWarScores -> AnalyzedMemberWarScores
generifyAllianceWarScores allianceName allianceWarScores =
  { pseudo = allianceName
  , averageScore = AverageMemberScore allianceWarScores.averageWarScore 0
  , maxScore = Just allianceWarScores.maxWarScore
  , participation = 100 -- The alliance always participates
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

viewGenericTitanScores : Translations -> AnalyzedMemberTitanScores -> List (Html Msg)
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

viewGenericWarScores : Translations -> AnalyzedMemberWarScores -> List (Html Msg)
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
          th [] [ text translations.teamValue ],
          th [] [ text translations.participation ]
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

participationsTitle : Translations -> { r | titanParticipation: Float, warParticipation: Float } -> String
participationsTitle translations { titanParticipation, warParticipation } =
  String.join "" [
    translations.titans, ": ", titanParticipation * 100 |> round |> String.fromInt, "%",
    "\n",
    translations.wars, ": ", warParticipation * 100 |> round |> String.fromInt, "%"
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
       ] [ text <| presentNumber <| round memberStats.teamValue ],
    td [ class "member-value", title (participationsTitle translations memberStats) ] [ viewParticipation memberStats.participation ]
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

filterAllianceTitanScores : StatsFilterExtender r -> List AllianceTitanScore -> AnalyzedAllianceTitanScores
filterAllianceTitanScores statsFilter allianceTitanScores =
  List.filter ( titanColorPredicate statsFilter ) allianceTitanScores
    |> List.filter ( titanStarsPredicate statsFilter )
    |> takeLast statsFilter.filteredTitanPeriod
    |> analyzeAllianceTitanScore

analyzeAllianceTitanScore : List AllianceTitanScore -> AnalyzedAllianceTitanScores
analyzeAllianceTitanScore allianceTitanScores =
  let
    preferredTitanColor : TitanColor
    preferredTitanColor = findPreferredEventType2 .titanColor .damage allianceTitanScores |> withDefault ALL

  in
    { averageTitanScore = computeAverageDamage allianceTitanScores
    , maxTitanScore = List.map .damage allianceTitanScores |> List.maximum |> Maybe.withDefault 0
    , progression = List.map .damage allianceTitanScores
        |> List.indexedMap (\index damage -> [index, damage])
        |> computeLinearRegression 2
    , preferredTitanColor = preferredTitanColor
    , titanScores = allianceTitanScores
    }

computeParticipation : List Int -> Float
computeParticipation damages =
  let
    hitsNumber : Int
    hitsNumber = List.filter (\damage -> damage /= 0) damages |> List.length

    eventsNumber : Int
    eventsNumber = List.length damages
  in
    (toFloat hitsNumber) / (toFloat eventsNumber)

warBonusPredicate : StatsFilterExtender r -> { u | warBonus : WarBonus } -> Bool
warBonusPredicate { filteredWarBonus } { warBonus } = filteredWarBonus == AllBonus || warBonus == filteredWarBonus

filterAllianceWarScores : StatsFilterExtender r -> List AllianceWarScore -> AnalyzedAllianceWarScores
filterAllianceWarScores statsFilter allianceWarScores =
  List.filter ( warBonusPredicate statsFilter ) allianceWarScores
    |> takeLast statsFilter.filteredWarPeriod
    |> analyzeAllianceWarScores

analyzeAllianceWarScores : List AllianceWarScore -> AnalyzedAllianceWarScores
analyzeAllianceWarScores allianceWarScores =
  let
    preferredWarBonus : WarBonus
    preferredWarBonus = findPreferredEventType2 .warBonus .damage allianceWarScores |> withDefault AllBonus

  in
    { averageWarScore = computeAverageDamage allianceWarScores
    , maxWarScore = List.map .damage allianceWarScores |> List.maximum |> Maybe.withDefault 0
    , progression = List.map .damage allianceWarScores
        |> List.indexedMap (\index damage -> [index, damage])
        |> computeLinearRegression 2
    , preferredWarBonus = preferredWarBonus
    , warScores = allianceWarScores
    }

filterMemberTitanScores : StatsFilterExtender r -> MemberTitanScores -> AnalyzedMemberTitanScores
filterMemberTitanScores statsFilter memberTitanScores =
    List.filter ( titanColorPredicate statsFilter ) memberTitanScores.titanScores
      |> List.filter ( titanStarsPredicate statsFilter )
      |> takeLast statsFilter.filteredTitanPeriod
      |> analyzeMemberTitanScoreList memberTitanScores.pseudo

analyzeMemberTitanScores : MemberTitanScores -> AnalyzedMemberTitanScores
analyzeMemberTitanScores memberTitanScores = analyzeMemberTitanScoreList memberTitanScores.pseudo memberTitanScores.titanScores

analyzeMemberTitanScoreList : String -> List MemberTitanScore -> AnalyzedMemberTitanScores
analyzeMemberTitanScoreList pseudo memberTitanScores =
  let
    preferredTitanColor : Maybe TitanColor
    preferredTitanColor = findPreferredEventType2 .titanColor ( .damage << .score ) memberTitanScores

    scores : List MemberScore
    scores = List.map .score memberTitanScores

    damages : List Int
    damages = List.map .damage scores

  in
    { pseudo = pseudo
    , averageScore = computeAverageScore scores
    , maxScore = List.maximum damages
    , participation = computeParticipation damages
    , progression = List.indexedMap (\index damage -> [index, damage] ) damages
      |> computeLinearRegression 2
    , preferredTitanColor = preferredTitanColor
    , titanScores = memberTitanScores
    }

filterMemberWarScores : StatsFilterExtender r -> MemberWarScores -> AnalyzedMemberWarScores
filterMemberWarScores statsFilter memberWarScores =
  List.filter ( warBonusPredicate statsFilter ) memberWarScores.warScores
      |> takeLast statsFilter.filteredWarPeriod
      |> analyzeMemberWarScoreList memberWarScores.pseudo

analyzeMemberWarScores : MemberWarScores -> AnalyzedMemberWarScores
analyzeMemberWarScores memberWarScores = analyzeMemberWarScoreList memberWarScores.pseudo memberWarScores.warScores

analyzeMemberWarScoreList : String -> List MemberWarScore -> AnalyzedMemberWarScores
analyzeMemberWarScoreList pseudo memberWarScores =
  let
    preferredWarBonus : Maybe WarBonus
    preferredWarBonus = findPreferredEventType2 .warBonus ( .damage << .score ) memberWarScores

    scores : List MemberScore
    scores = List.map .score memberWarScores

    damages : List Int
    damages = List.map .damage scores

  in
    { pseudo = pseudo
    , averageScore = computeAverageScore scores
    , maxScore = List.maximum damages
    , participation = computeParticipation damages
    , progression = List.indexedMap (\index damage -> [index, damage] ) damages
      |> computeLinearRegression 2
    , preferredWarBonus = preferredWarBonus
    , warScores = memberWarScores
    }

analyzeAllianceStats : Stats -> AnalyzedStats
analyzeAllianceStats stats =
  { allianceTitanScores = analyzeAllianceTitanScore stats.allianceTitanScores
  , allianceWarScores = analyzeAllianceWarScores stats.allianceWarScores
  , membersTitanScores = List.map analyzeMemberTitanScores stats.membersTitanScores
  , membersWarScores = List.map analyzeMemberWarScores stats.membersWarScores
  }

filterStats : StatsFilterExtender r -> Stats -> AnalyzedStats
filterStats statsFilter stats =
  { allianceTitanScores = filterAllianceTitanScores statsFilter stats.allianceTitanScores
  , allianceWarScores = filterAllianceWarScores statsFilter stats.allianceWarScores
  , membersTitanScores = List.map ( filterMemberTitanScores statsFilter ) stats.membersTitanScores
  , membersWarScores = List.map ( filterMemberWarScores statsFilter ) stats.membersWarScores
  }

decodeStats : String -> RawStats
decodeStats statsAsString = decodeRawStats statsAsString

retrieveMemberWarScores : Int -> List ( AnalyzedMemberWarScores ) -> AnalyzedMemberWarScores
retrieveMemberWarScores index memberWarScoresList = Maybe.withDefault
  ( AnalyzedMemberWarScores "" ( AverageMemberScore 0 0 ) Nothing 0.0 (RegressionResult 0 0 (\_ -> 0)) Nothing [] ) -- Can't fail anyway, checked before that
  ( getAt index memberWarScoresList )

mergeTeamValues : AverageMemberScore -> AverageMemberScore -> Float
mergeTeamValues titanAverageScore warAverageScore = ( titanAverageScore.teamValue + warAverageScore.teamValue ) / 2

computeMemberStats : (String, AnalyzedMemberTitanScores, AnalyzedMemberWarScores) -> ( String, MemberStats )
computeMemberStats ( pseudo, memberTitanScores, memberWarScores ) =
  (
    pseudo,
    { pseudo = pseudo
    , averageTitanScore = memberTitanScores.averageScore
    , maxTitanScore = memberTitanScores.maxScore
    , titanParticipation = memberTitanScores.participation
    , preferredTitanColor = memberTitanScores.preferredTitanColor
    , averageWarScore = memberWarScores.averageScore
    , warParticipation = memberWarScores.participation
    , preferredWarBonus = memberWarScores.preferredWarBonus
    , participation = (memberTitanScores.participation + memberWarScores.participation) / 2
    , titanTeamValue = memberTitanScores.averageScore.teamValue
    , warTeamValue = memberWarScores.averageScore.teamValue
    , teamValue = mergeTeamValues memberTitanScores.averageScore memberWarScores.averageScore
    }
  )

computeAllianceStats : AnalyzedStats -> AllianceStats
computeAllianceStats analyzedStats =
  { averageTitanScore = analyzedStats.allianceTitanScores.averageTitanScore
  , maxTitanScore = analyzedStats.allianceTitanScores.maxTitanScore
  , preferredTitanColor = analyzedStats.allianceTitanScores.preferredTitanColor
  , averageWarScore = analyzedStats.allianceWarScores.averageWarScore
  , maxWarScore = analyzedStats.allianceWarScores.maxWarScore
  , preferredWarBonus = analyzedStats.allianceWarScores.preferredWarBonus
  , memberStats = List.indexedMap
      (\index titanScores -> ( titanScores.pseudo, titanScores, retrieveMemberWarScores index analyzedStats.membersWarScores ) ) analyzedStats.membersTitanScores
      |> List.map computeMemberStats
      |> Dict.fromList
  }

updateValidStats : Stats -> Model r -> Model r
updateValidStats stats model =
  let
    defaultStatsFilter : StatsFilter
    defaultStatsFilter = createDefaultStatsFilter model.translations

    filteredStats : AnalyzedStats
    filteredStats = filterStats defaultStatsFilter stats

    allianceStats : AllianceStats
    allianceStats = computeAllianceStats <| analyzeAllianceStats stats
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

