module Main exposing (main)

import Browser
import Http

import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing (interpolate)


computeRequestUrl : String -> String -> String
computeRequestUrl range apiKey =
    interpolate "https://sheets.googleapis.com/v4/spreadsheets/19eKYnYjibyZ2syrr66g4-gR6SIgF_pCHjUWWYiqSpJg/values/{0}?key={1}" [ range, apiKey ]

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type Model
  = LoadingFailure
  | Loading
  | Success String

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = computeRequestUrl "Titans!A%3AAZ" "tooSensitiveToShare" -- TODO: Variabelize
      , expect = Http.expectString GotText
      }
  )

--UPDATE

type Msg
  = GotText (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (LoadingFailure, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  case model of
    LoadingFailure ->
      text "I was unable to load your sheet."

    Loading ->
      text "Loading..."

    Success fullText ->
      pre [] [ text fullText ]
