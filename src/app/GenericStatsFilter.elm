module GenericStatsFilter exposing (GenericStatsFilterExtender, viewGenericFilterForm)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, min, max, value)
import Html.Events exposing (onInput)

import Msg exposing (..)
import Optionize exposing (optionize)

-----------
-- MODEL --
-----------

type alias GenericStatsFilterExtender r =
  { r
  | filteredMember : String
  , filteredPeriod : Int
  }

----------
-- VIEW --
----------

viewGenericFilterForm : GenericStatsFilterExtender r -> List String -> Html Msg
viewGenericFilterForm genericStatsFilter members =
  let
    filteredPeriod : String
    filteredPeriod = String.fromInt genericStatsFilter.filteredPeriod

  in
    Html.form [ class "generic-stat-filters" ]
      [ h2 [] [ text "Filter the stats" ]
      , div [ class "form-field-inline" ]
          [ label [ for "member" ] [ text "Member" ]
          , select
              [ id "member"
              , onInput ( StatsMsg << NewMemberSelected )
              ]
              ( optionize genericStatsFilter.filteredMember members )
          ]
      , div [ class "form-field-inline" ]
          [ label [ for "period" ] [ text "Period" ]
          , input
              [ id "period"
              , type_ "number"
              , min "10"
              , max "120"
              , value filteredPeriod
              , onInput ( StatsMsg << NewPeriodSelected )
              ]
              []
          ]
      ]

