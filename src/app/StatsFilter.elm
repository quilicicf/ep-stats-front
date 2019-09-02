module StatsFilter exposing (
    StatsFilter, StatsFilterExtender,
    createDefaultStatsFilter,
    viewTitansFilterForm, viewWarsFilterForm,
    updateStatsFilters
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)

import Msg exposing (..)
import Optionize exposing (..)
import StepRange exposing (..)
import Translations exposing (..)
import Titans exposing (..)
import Wars exposing (..)

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

defaultFilterPeriod : Int
defaultFilterPeriod = 30

createDefaultStatsFilter : Translations -> StatsFilter
createDefaultStatsFilter translations =
  { filteredMember = translations.alliance
  , filteredTitanPeriod = defaultFilterPeriod
  , filteredTitanColor = allTitanColors
  , filteredTitanStars = Nothing
  , filteredWarPeriod = defaultFilterPeriod
  , filteredWarBonus = allWarBonuses
  }

----------
-- VIEW --
----------

type alias Model r = StatsFilterExtender (TranslationsExtender r)

allPeriods : List String
allPeriods = stepRange 10 120 10 |> List.map String.fromInt

viewTitansFilterForm : Model r -> List String -> Html Msg
viewTitansFilterForm model members =
  let
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
            ( optionizeStrings model.filteredMember members )
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "period" ] [ text model.translations.period ]
        , select
            [ id "period"
            , value <| String.fromInt model.filteredTitanPeriod
            , onInput ( StatsFilterMsg << NewTitanPeriodSelected << ( withDefault defaultFilterPeriod ) << String.toInt )
            ]
            ( optionizeStrings ( String.fromInt model.filteredTitanPeriod ) ( allPeriods ) )
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "color" ] [ text model.translations.color ]
        , select
            [ id "color" , onInput colorFilterGuesser ]
            ( optionizeObjects .code titanColorNameExtractor model.filteredTitanColor titanColors )
        ]
    , div [ class "form-field-inline" ]
        [ label [ for "stars" ] [ text model.translations.stars ]
        , select
            [ id "stars"
            , onInput ( StatsFilterMsg << NewTitanStarsSelected << String.toInt )
            ]
            ( optionizeObjects
              .value
              .label
              ( newStarsOption model.translations model.filteredTitanStars )
              ( allStarsOptions model.translations )
            )
        ]
    ]

colorFilterGuesser : String -> Msg
colorFilterGuesser colorFilterAsString = ( StatsFilterMsg << NewTitanColorSelected << titanColorFromString ) colorFilterAsString

type alias StarsOption = { label : String, value : String }

newStarsOption : Translations -> Maybe Int -> StarsOption
newStarsOption translations maybeStars =
  case maybeStars of
    Just stars -> StarsOption ( String.fromInt stars ) ( String.fromInt stars )
    Nothing -> StarsOption translations.all "ALL"

allStarsOptions : Translations -> List StarsOption
allStarsOptions translations = ( Nothing :: ( List.range 1 12 |> List.map Just ) )
 |> List.map ( newStarsOption translations )

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
              ( optionizeStrings model.filteredMember members )
          ]
      , div [ class "form-field-inline" ]
          [ label [ for "period" ] [ text model.translations.period ]
          , select
              [ id "period"
              , value <| String.fromInt model.filteredWarPeriod
              , onInput ( StatsFilterMsg << NewWarPeriodSelected << ( withDefault defaultFilterPeriod ) << String.toInt )
              ]
              ( optionizeStrings ( model.filteredWarPeriod |> String.fromInt ) ( allPeriods ) )
          ]
      , div [ class "form-field-inline" ]
          [ label [ for "bonus" ] [ text model.translations.bonus ]
          , select
              [ id "bonus" , onInput bonusFilterGuesser ]
              ( optionizeObjects .code warBonusNameExtractor model.filteredWarBonus warBonuses )
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



