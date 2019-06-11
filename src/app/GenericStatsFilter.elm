module GenericStatsFilter exposing (GenericStatsFilter, defaultGenericStatsFilter,
  viewGenericFilterForm)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Msg exposing (..)
import Optionize exposing (optionize)
import AllianceName exposing (allianceName)

-----------
-- MODEL --
-----------

type alias GenericStatsFilter =
  { user : String
  , period : Int
  }

-----------
-- UTILS --
-----------

defaultGenericStatsFilter : GenericStatsFilter
defaultGenericStatsFilter = GenericStatsFilter allianceName 30

----------
-- VIEW --
----------

viewGenericFilterForm : GenericStatsFilter -> List String -> Html Msg
viewGenericFilterForm genericStatsFilter members =
  Html.form [ class "generic-stat-filters" ]
    [ h2 [] [ text "Filter the stats" ]
    , div [ class "form-field" ]
        [ label [ for "member" ] [ text "Member" ]
        , select
            [ id "member"
            , onInput ( StatsMsg << NewMemberSelected )
            ]
            ( optionize genericStatsFilter.user members )
        ]
    ]
