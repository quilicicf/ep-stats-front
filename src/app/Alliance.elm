module Alliance exposing (viewAlliance)

import Dict exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Maybe exposing (withDefault)

import Msg exposing (..)
import WarStats exposing (WarStats)
import Stats exposing (StatsExtender)
import TitanStats exposing (TitanStats, TitanColor, MemberTitanScore)

------------
-- MODELS --
------------

type alias MemberRepresentation =
  { pseudo : String
  , averageTitanScore : Float
--  , preferredTitanColor : TitanColor
  , averageWarScore : Float
--  , preferredWarBonus : String
  , teamValue : Float
  }

-----------
-- UTILS --
-----------

createMemberRepresentations : TitanStats -> WarStats -> List String -> List MemberRepresentation
createMemberRepresentations titanStats warStats members =
  List.map ( createMemberRepresentation titanStats warStats ) members

createMemberRepresentation : TitanStats -> WarStats -> String -> MemberRepresentation
createMemberRepresentation titanStats warStats pseudo =
  let
    memberTitanScores : List MemberTitanScore
    memberTitanScores = withDefault ( [] ) ( Dict.get pseudo titanStats.titanScores )

    titanTotalDamage : Int
    titanTotalDamage = List.map .score memberTitanScores
       |> List.map ( withDefault 0 )
       |> List.foldl (+) 0

    titanAverageScore : Float
    titanAverageScore = ( toFloat titanTotalDamage ) / ( toFloat ( List.length memberTitanScores ) )

  in
    MemberRepresentation
      pseudo
      ( titanAverageScore )
      ( 0.0 )
      ( 0.0 )

----------
-- VIEW --
----------

viewAlliance : StatsExtender r -> Html Msg
viewAlliance stats =
  let
    titanStats : TitanStats
    titanStats = withDefault ( TitanStats [] Dict.empty ) stats.titanStats

    warStats : WarStats
    warStats = withDefault ( WarStats [] [] ) stats.warStats

    members : List String
    members = Dict.keys titanStats.titanScores

    memberRepresentations : List MemberRepresentation
    memberRepresentations = createMemberRepresentations titanStats warStats members

  in
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
          List.map viewMember memberRepresentations
            |> List.drop 1 -- Drop alliance row
        )
      ]
    ]

viewMember : MemberRepresentation -> Html Msg
viewMember memberRepresentation =
  tr [ class "member-row" ] [
    th [ class "member-pseudo" ] [ text memberRepresentation.pseudo ],
    td [ class "member-value" ] [ text ( String.fromFloat memberRepresentation.averageTitanScore ) ],
    td [] [],
    td [] [],
    td [] [],
    td [] []
  ]
