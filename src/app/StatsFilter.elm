module StatsFilter exposing (StatsFilterExtender, defaultStatsFilter, viewTitanFilterForm)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, value, min, max, step)
import Html.Events exposing (onInput)

import Msg exposing (..)
import Optionize exposing (optionize)
import AllianceName exposing (allianceName)
import Titans exposing (DetailedColor)

-----------
-- MODEL --
-----------

type alias StatsFilterExtender r =
  { r
  -- Generic
  | filteredMember : String
  -- Titans
  , filteredTitanPeriod : Int
  , filteredTitanColor : Maybe DetailedColor
  , filteredTitanStars : Maybe Int
  -- Wars
  , filteredWarPeriod : Int
  , filteredWarBonus : Maybe String
  }

type alias StatsFilter = StatsFilterExtender {}

-----------
-- UTILS --
-----------

defaultStatsFilter : StatsFilter
defaultStatsFilter =
  { filteredMember = allianceName
  , filteredTitanPeriod = 30
  , filteredTitanColor = Nothing
  , filteredTitanStars = Nothing
  , filteredWarPeriod = 30
  , filteredWarBonus = Nothing
  }

----------
-- VIEW --
----------

viewTitanFilterForm : StatsFilterExtender r -> List String -> Html Msg
viewTitanFilterForm statsFilter members =
  let
    filteredPeriod : String
    filteredPeriod = String.fromInt statsFilter.filteredTitanPeriod

  in
    Html.form [ class "generic-stat-filters" ]
      [ h2 [] [ text "Filter the stats" ]
      , div [ class "form-field-inline" ]
          [ label [ for "member" ] [ text "Member" ]
          , select
              [ id "member"
              , onInput ( StatsMsg << NewMemberSelected )
              ]
              ( optionize statsFilter.filteredMember members )
          ]
      , div [ class "form-field-inline" ]
          [ label [ for "period" ] [ text "Period" ]
          , input
              [ id "period"
              , type_ "number"
              , min "10"
              , max "120"
              , step "10"
              , value filteredPeriod
              , onInput ( StatsMsg << NewPeriodSelected )
              ]
              []
          ]
      ]

