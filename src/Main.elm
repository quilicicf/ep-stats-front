module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)

import Url exposing (Url)

import Json.Decode as Decode exposing (Value)

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url

type alias AppConfig =
  { teamName: String
  , sheetId: String
  , apiKey: String
  }

type Page
  = WelcomePage
  | AppConfigPage
  | AppKeyPage
  | NotFoundPage

type alias Model =
  { appConfig: AppConfig
  , currentPage: Page
  , navigationKey: Key
  }

welcomeScreen : Model -> Html Msg
welcomeScreen model =
  div []
    [ text "Choose your side"
    , a [ href "/appConfig" ] [ text "I'm the alliance's GOD" ]
    , a [ href "/appKey" ] [ text "I'm but a peon" ]
    ]


appConfigForm : AppConfig -> Html Msg
appConfigForm appConfig =
  Html.form []
    [ div []
        [ text "Team name"
        , br [] []
        , input
            [ type_ "text"
            , value appConfig.teamName
            ]
            []
        ]
    , br [] []
    , div []
        [ text "Sheet id"
        , br [] []
        , input
            [ type_ "text"
            , value appConfig.sheetId
            ]
            []
        ]
    , br [] []
    , div []
        [ text "API key"
        , br [] []
        , input
            [ type_ "text"
            , value appConfig.apiKey
            ]
            []
        ]
    , br [] []
    , div []
        [ button
            [ type_ "button" ]
            [ text "Create" ]
        ]
    ]

init : Value -> Url -> Key -> (Model, Cmd Msg)
init _ url key =
  let
      initPage : Page
      initPage = findPage url

      initModel: Model
      initModel = Model (AppConfig "" "" "") (initPage) key
  in
    ( initModel , Cmd.none )

findPage : Url -> Page
findPage url =
  case url.path of
    "/" -> WelcomePage
    "/appConfig" -> AppConfigPage
    "/appKey" -> AppKeyPage
    _ -> NotFoundPage

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url -> ( model, pushUrl model.navigationKey (Url.toString url) )

        Browser.External href -> ( model, load href )

    UrlChanged url ->
      let
        newPage: Page
        newPage= findPage url
      in
        log url.path
        ( { model | currentPage = newPage }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Document Msg
view model =
  case model.currentPage of
    WelcomePage -> (
      { title = "EP stats"
      , body = [
          div
            []
            [ h1 [] [ text "EP stats" ]
            , welcomeScreen model
            ]
        ]
      })

    AppConfigPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "EP stats" ]
            , appConfigForm model.appConfig
            ]
        ]
      })

    AppKeyPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "EP stats" ]
            , appConfigForm model.appConfig
            ]
        ]
      })

    NotFoundPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "Not found" ] ]
        ]
      })



main : Program Value Model Msg
main = application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = UrlChanged
  , onUrlRequest = LinkClicked
  }

