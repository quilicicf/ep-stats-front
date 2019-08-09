module StatsFilter exposing (StatsFilterExtender, defaultStatsFilter, viewTitansFilterForm, viewWarsFilterForm, updateStatsFilters)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, value, min, max, step)
import Html.Events exposing (onInput)
import Maybe exposing (..)

import Msg exposing (..)
import Optionize exposing (optionize)
import SafeParseInt exposing (safeParseInt)
import AllianceName exposing (allianceName)
import Titans exposing (DetailedColor, titanColorFromString, titanColors, allTitanColors)
import Wars exposing (warBonuses, warBonusFromString)

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
            , onInput ( StatsFilterMsg << NewTitanPeriodSelected << ( withDefault defaultStatsFilter.filteredTitanPeriod ) << safeParseInt )
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
  else ( StatsFilterMsg << NewTitanStarsSelected << safeParseInt ) starsFilterAsString

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
            , value ( String.fromInt statsFilter.filteredTitanPeriod )
            , onInput ( StatsFilterMsg << NewTitanPeriodSelected << ( withDefault defaultStatsFilter.filteredTitanPeriod ) << safeParseInt )
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
              ( withDefault allBonusesFilter statsFilter.filteredWarBonus )
              ( allBonusesFilter :: warBonuses )
            )
        ]
    ]

allBonusesFilter : String
allBonusesFilter = "ALL"

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

    NewTitanStarsSelected starsFilter -> { model | filteredTitanStars = starsFilter }

    NewWarBonusSelected warBonusFilter -> { model | filteredWarBonus = warBonusFilter }



