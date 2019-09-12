port module Main exposing (..)

import Browser exposing (..)
import Browser.Navigation exposing (..)

import CustomStyle exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Maybe exposing (..)
import PrivacyPolicy exposing (..)
import Url exposing (..)

type alias Model = { greetings: String }

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url

init : Value -> Url -> Key -> (Model, Cmd Msg)
init _ _ key = ( { greetings = "Hello world" }, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = ( model, Cmd.none )

view : Model -> Document Msg
view model =
  { title = "Hello"
  , body = [
      div [] [ text model.greetings ]
    ]
  }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main : Program Value Model Msg
main = application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = UrlChanged
  , onUrlRequest = LinkClicked
  }

