module Main exposing (main)

import Browser
import Http

import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing (interpolate)

import Json.Decode as Decode exposing (Value)

import AppConfigForm exposing (AppConfig, main)

computeRequestUrl : String -> String -> String
computeRequestUrl range apiKey =
    interpolate "https://sheets.googleapis.com/v4/spreadsheets/19eKYnYjibyZ2syrr66g4-gR6SIgF_pCHjUWWYiqSpJg/values/{0}?key={1}" [ range, apiKey ]

-- MAIN

main =
  Browser.document
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


-- MODEL

type alias Key = String

type Model
  = Nothing
  | AppConfig
  | Key
--  | LoadingFailure
--  | Loading
--  | Success String

init : Value -> (Model, Cmd msg)
init flags = (Nothing, Cmd.none)

--load : () -> (Model, Cmd Msg)
--load _ =
--  ( Loading
--  , Http.get
--      { url = computeRequestUrl "Titans!A%3AAZ" "tooSensitiveToShare" -- TODO: Variabelize
--      , expect = Http.expectString GotText
--      }
--  )

--UPDATE

type Msg
  = ChangedUrl String
  | ClickedLink String
--  GotText (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangedUrl newUrl -> (model , Cmd.none)
    ClickedLink link -> (model , Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

createAppTitle: String -> Html msg
createAppTitle title = h1 [ class "app-title" ] [
    img [ src "/assets/icon.png", alt "ep-stats icon" ] [],
    text title
  ]

view : Model -> Browser.Document Msg
view model =
    let
        appBody: Html msg
        appBody =  case model of
          Nothing -> main

          Key -> text "Enter the key"

          AppConfig -> text "Enter the alliance info"

    in
      { title = "EP-stats home"
      , body =
        [
          div [] [
            createAppTitle "EP-stats",
            appBody
          ]
        ]
      }
--
--    LoadingFailure ->
--      text "I was unable to load your sheet."
--
--    Loading ->
--      text "Loading..."
--
--    Success fullText ->
--      pre [] [ text fullText ]
