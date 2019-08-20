module StatsFilter exposing (StatsFilterExtender, defaultStatsFilter, viewTitansFilterForm, viewWarsFilterForm, updateStatsFilters)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, value, min, max, step)
import Html.Events exposing (onInput)
import Maybe exposing (..)

import Msg exposing (..)
import Optionize exposing (optionize)
import AllianceName exposing (allianceName)
import Titans exposing (DetailedColor, titanColorFromString, titanColors, allTitanColors)
import Wars exposing (WarBonus, warBonuses, allWarBonuses, warBonusFromString)

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
  , filteredWarBonus : WarBonus
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
  , filteredWarBonus = allWarBonuses
  }

----------
-- VIEW --
----------

viewTitansFilterForm : StatsFilterExtender r -> List String -> Html Msg
viewTitansFilterForm statsFilter members =
  Html.form [ class "stat-filters" ]
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
            , onInput ( StatsFilterMsg << NewTitanPeriodSelected << ( withDefault defaultStatsFilter.filteredTitanPeriod ) << String.toInt )
            ]
            []
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "color" ] [ text "Color" ]
        , select
            [ id "color"
            , onInput colorFilterGuesser
            ]
            ( optionize
              ( statsFilter.filteredTitanColor |> .name )
              ( List.map .name titanColors )
            )
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "stars" ] [ text "Stars" ]
        , select
            [ id "stars"
            , onInput starsFilterGuesser
            ]
            ( optionize
              ( Maybe.map String.fromInt statsFilter.filteredTitanStars |> withDefault allStarsFilter )
              ( starsOptions )
            )
        ]
    ]

colorFilterGuesser : String -> Msg
colorFilterGuesser colorFilterAsString = ( StatsFilterMsg << NewTitanColorSelected << titanColorFromString ) colorFilterAsString

starsOptions : List String
starsOptions = allStarsFilter :: ( List.map String.fromInt [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] )

allStarsFilter : String
allStarsFilter = "ALL"

starsFilterGuesser : String -> Msg
starsFilterGuesser starsFilterAsString =
  if starsFilterAsString == allStarsFilter
  then ( StatsFilterMsg << NewTitanStarsSelected ) Nothing
  else ( StatsFilterMsg << NewTitanStarsSelected << String.toInt ) starsFilterAsString

viewWarsFilterForm : StatsFilterExtender r -> List String -> Html Msg
viewWarsFilterForm statsFilter members =
  Html.form [ class "stat-filters" ]
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
            , value ( String.fromInt statsFilter.filteredWarPeriod )
            , onInput ( StatsFilterMsg << NewWarPeriodSelected << ( withDefault defaultStatsFilter.filteredWarPeriod ) << String.toInt )
            ]
            []
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "bonus" ] [ text "Bonus" ]
        , select
            [ id "bonus"
            , onInput bonusFilterGuesser
            ]
            ( optionize
              ( statsFilter.filteredWarBonus |> .name )
              ( List.map .name warBonuses )
            )
        ]
    ]

bonusFilterGuesser : String -> Msg
bonusFilterGuesser bonusFilterAsString = ( StatsFilterMsg << NewWarBonusSelected << warBonusFromString ) bonusFilterAsString

------------
-- UPDATE --
------------

updateStatsFilters : StatsFilterMsg -> StatsFilterExtender r -> StatsFilterExtender r
updateStatsFilters msg model =
  case msg of
    NewMemberSelected newSelectedMember ->
      { model | filteredMember = newSelectedMember }

    NewTitanPeriodSelected newTitanPeriod ->
      { model | filteredTitanPeriod = newTitanPeriod }

    NewTitanColorSelected newTitanColor ->
      { model | filteredTitanColor = newTitanColor }

    NewWarPeriodSelected newWarPeriod ->
      { model | filteredWarPeriod = newWarPeriod }

    NewTitanStarsSelected starsFilter -> { model | filteredTitanStars = starsFilter }

    NewWarBonusSelected warBonusFilter -> { model | filteredWarBonus = warBonusFilter }



