module StatsFilter exposing (StatsFilter, StatsFilterExtender, createDefaultStatsFilter, viewTitansFilterForm, viewWarsFilterForm, updateStatsFilters)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, value, min, max, step)
import Html.Events exposing (onInput)
import Maybe exposing (..)

import Msg exposing (..)
import Optionize exposing (optionize)
import Translations exposing (Translations, TranslationsExtender)
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

createDefaultStatsFilter : Translations -> StatsFilter
createDefaultStatsFilter translations =
  { filteredMember = translations.alliance
  , filteredTitanPeriod = 30
  , filteredTitanColor = allTitanColors
  , filteredTitanStars = Nothing
  , filteredWarPeriod = 30
  , filteredWarBonus = allWarBonuses
  }

----------
-- VIEW --
----------

type alias Model r = StatsFilterExtender (TranslationsExtender r)

viewTitansFilterForm : Model r -> List String -> Html Msg
viewTitansFilterForm model members =
  let
    defaultStatsFilter : StatsFilter
    defaultStatsFilter = createDefaultStatsFilter model.translations

    titanColorNameExtractor : DetailedColor -> String
    titanColorNameExtractor color = color.nameGetter model.translations
  in
  Html.form [ class "stat-filters" ]
    [ h2 [] [ text model.translations.filters ]
    , div [ class "form-field-inline" ]
        [ label [ for "member" ] [ text model.translations.member ]
        , select
            [ id "member"
            , onInput ( StatsFilterMsg << NewMemberSelected )
            ]
            ( optionize model.filteredMember members )
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "period" ] [ text model.translations.period ]
        , input
            [ id "period"
            , type_ "number"
            , min "10"
            , max "120"
            , step "10"
            , value ( String.fromInt model.filteredTitanPeriod )
            , onInput ( StatsFilterMsg << NewTitanPeriodSelected << ( withDefault defaultStatsFilter.filteredTitanPeriod ) << String.toInt )
            ]
            []
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "color" ] [ text model.translations.color ]
        , select
            [ id "color"
            , onInput colorFilterGuesser
            ]
            ( optionize
              ( model.filteredTitanColor |> titanColorNameExtractor )
              ( List.map titanColorNameExtractor titanColors )
            )
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "stars" ] [ text model.translations.stars ]
        , select
            [ id "stars"
            , onInput starsFilterGuesser
            ]
            ( optionize
              ( Maybe.map String.fromInt model.filteredTitanStars |> withDefault allStarsFilter )
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

viewWarsFilterForm : Model r -> List String -> Html Msg
viewWarsFilterForm model members =
  let
    defaultStatsFilter : StatsFilter
    defaultStatsFilter = createDefaultStatsFilter model.translations

    warBonusNameExtractor : WarBonus -> String
    warBonusNameExtractor bonus = bonus.nameGetter model.translations
  in
    Html.form [ class "stat-filters" ]
      [ h2 [] [ text model.translations.filters ]
      , div [ class "form-field-inline" ]
          [ label [ for "member" ] [ text model.translations.member ]
          , select
              [ id "member"
              , onInput ( StatsFilterMsg << NewMemberSelected )
              ]
              ( optionize model.filteredMember members )
          ]
      , div [ class "form-field-inline" ]
          [ label [ for "period" ] [ text model.translations.period ]
          , input
              [ id "period"
              , type_ "number"
              , min "10"
              , max "120"
              , step "10"
              , value ( String.fromInt model.filteredWarPeriod )
              , onInput ( StatsFilterMsg << NewWarPeriodSelected << ( withDefault defaultStatsFilter.filteredWarPeriod ) << String.toInt )
              ]
              []
          ]
      , div [ class "form-field-inline" ]
          [ label [ for "bonus" ] [ text model.translations.bonus ]
          , select
              [ id "bonus"
              , onInput bonusFilterGuesser
              ]
              ( optionize
                ( model.filteredWarBonus |> warBonusNameExtractor )
                ( List.map warBonusNameExtractor warBonuses )
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



