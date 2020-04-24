module Spinner exposing (viewSpinner)

import Html exposing (..)
import Html.Attributes exposing (..)

import Msg exposing (..)
import CustomStyle exposing (..)

viewSpinner : String -> Html Msg
viewSpinner message =
  div [ class "spinner-container" ] [
    span [ class "spinner-text" ] [ text message ],
    div [ class "spinner" ] [
      div [ class "bar", customStyle [ ("--bar-index", "0"), ("--bar-color", "var(--ep-fire") ] ] [],
      div [ class "bar", customStyle [ ("--bar-index", "1"), ("--bar-color", "var(--ep-nature") ] ] [],
      div [ class "bar", customStyle [ ("--bar-index", "2"), ("--bar-color", "var(--ep-ice") ] ] [],
      div [ class "bar", customStyle [ ("--bar-index", "3"), ("--bar-color", "var(--ep-holy") ] ] [],
      div [ class "bar", customStyle [ ("--bar-index", "4"), ("--bar-color", "var(--ep-dark") ] ] []
    ]
  ]
