module StatsFilter exposing (StatsFilterExtender, defaultStatsFilter, viewTitanFilterForm, updateStatsFilters)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, value, min, max, step)
import Html.Events exposing (onInput)
import Maybe exposing (..)

import Msg exposing (..)
import Optionize exposing (optionize)
import AllianceName exposing (allianceName)
import Titans exposing (DetailedColor, titanColorFromString, titanColors, allTitanColors)

-----------
-- MODEL --
-----------

type alias StatsFilterExtender r =
  { r
  -- Generic
  | filteredMember : String
  -- Titans
  , filteredTitanPeriod : Int
  , filteredTitanColor : DetailedColor
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
  , filteredTitanColor = allTitanColors
  , filteredTitanStars = Nothing
  , filteredWarPeriod = 30
  , filteredWarBonus = Nothing
  }

----------
-- VIEW --
----------

viewTitanFilterForm : StatsFilterExtender r -> List String -> Html Msg
viewTitanFilterForm statsFilter members =
  Html.form [ class "generic-stat-filters" ]
    [ h2 [] [ text "Filter the stats" ]
    , div [ class "form-field-inline" ]
        [ label [ for "member" ] [ text "Member" ]
        , select
            [ id "member"
            , onInput ( StatsFilterMsg << NewMemberSelected )
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
            , value ( String.fromInt statsFilter.filteredTitanPeriod )
            , onInput ( StatsFilterMsg << NewTitanPeriodSelected )
            ]
            []
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "color" ] [ text "Color" ]
        , select
            [ id "color"
            , onInput ( StatsFilterMsg << NewTitanColorSelected )
            ]
            ( optionize
              ( statsFilter.filteredTitanColor |> .name )
              ( List.map .name titanColors )
            )
        ]
    ]

------------
-- UPDATE --
------------

updateStatsFilters : StatsFilterMsg -> StatsFilterExtender r -> StatsFilterExtender r
updateStatsFilters msg model =
  case msg of
    NewMemberSelected newSelectedMember ->
      { model | filteredMember = newSelectedMember }

    NewTitanPeriodSelected newTitanPeriodAsString ->
      { model | filteredTitanPeriod = Maybe.withDefault defaultStatsFilter.filteredTitanPeriod (String.toInt newTitanPeriodAsString) }

    NewTitanColorSelected newTitanColorAsString ->
      { model | filteredTitanColor = titanColorFromString newTitanColorAsString }

