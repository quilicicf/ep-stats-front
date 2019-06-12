module GenericStatsFilter exposing (GenericStatsFilterExtender, viewGenericFilterForm)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
    ]
