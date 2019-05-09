port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Url exposing (Url)

import Base64 exposing (..)

import Json.Decode exposing (Value)
import Json.Encode as Encode

port setStorage : AppConfig -> Cmd msg

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | NewTeamName String
  | NewSheetId String
  | NewApiKey String
  | CreateAppConfig

type alias AppConfig =
  { teamName: String
  , sheetId: String
  , apiKey: String
  }

type Page
  = WelcomePage
  | AppConfigPage
  | AppKeyPage
  | StatsPage
  | NotFoundPage

type alias Model =
  { appConfig: AppConfig
  , appKey: String
  , currentPage: Page
  , navigationKey: Key
  }


jsonifyAppConfig : AppConfig -> Value
jsonifyAppConfig appConfig =
  Encode.object
      [ ("teamName", Encode.string appConfig.teamName)
      , ("sheetId", Encode.string appConfig.sheetId)
      , ("apiKey", Encode.string appConfig.apiKey)
      ]

encodeAppConfig : AppConfig -> String
encodeAppConfig appConfig = appConfig
  |> jsonifyAppConfig
  |> Encode.encode 0
  |> Base64.encode

welcomeScreen : Model -> Html Msg
welcomeScreen _ =
  div []
    [ text "Choose your side"
    , br [] []
    , a [ href "/appConfig" ] [ text "I'm the alliance's GOD" ]
    , br [] []
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
            , onInput NewTeamName
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
            , onInput NewSheetId
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
            , onInput NewApiKey
            ]
            []
        ]
    , br [] []
    , div []
        [ button
            [ type_ "button", onClick CreateAppConfig ]
            [ text "Create" ]
        ]
    ]

appKeyForm : String -> Html Msg
appKeyForm appKey =
  Html.form []
    [ div []
        [ text "App key"
        , br [] []
        , input
            [ type_ "text"
            , value appKey
            ]
            []
        ]
    , br [] []
    , div []
        [ button
            [ type_ "button" ]
            [ text "See" ]
        ]
    ]

init : Value -> Url -> Key -> (Model, Cmd Msg)
init _ url key =
  let
      initPage : Page
      initPage = findPage url

      initModel: Model
      initModel = Model (AppConfig "" "" "") "" initPage key
  in
    ( initModel , Cmd.none )

findPage : Url -> Page
findPage url =
  case url.path of
    "/" -> WelcomePage
    "/appConfig" -> AppConfigPage
    "/appKey" -> AppKeyPage
    "/stats" -> StatsPage
    _ -> NotFoundPage

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
  let
    ( ({ appConfig } as newModel), cmds ) = update msg model
  in
    ( newModel, Cmd.batch [ setStorage appConfig, cmds ] )

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ appConfig } as model) =
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

    NewTeamName newTeamName ->
      let
        newAppConfig : AppConfig
        newAppConfig = { appConfig | teamName = newTeamName }
      in
        ( { model | appConfig = newAppConfig }, Cmd.none )

    NewSheetId newSheetId ->
      let
        newAppConfig : AppConfig
        newAppConfig = { appConfig | sheetId = newSheetId }
      in
        ( { model | appConfig = newAppConfig }, Cmd.none )

    NewApiKey newApiKey ->
      let
        newAppConfig : AppConfig
        newAppConfig = { appConfig | apiKey = newApiKey }
      in
        ( { model | appConfig = newAppConfig }, Cmd.none )

    CreateAppConfig ->
      let
        toto = log "Toto" appConfig
        newAppKey : String
        newAppKey = encodeAppConfig appConfig
      in
        ( { model | appKey = newAppKey }, pushUrl model.navigationKey "/stats" )

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
            , appKeyForm model.appKey
            ]
        ]
      })

    StatsPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "Stats should go here" ] ]
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
  , update = updateWithStorage
  , subscriptions = subscriptions
  , onUrlChange = UrlChanged
  , onUrlRequest = LinkClicked
  }

